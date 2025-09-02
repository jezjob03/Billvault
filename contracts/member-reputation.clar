;; Member Reputation System
;; Tracks member payment history, contribution reliability, and participation

(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_POOL_NOT_FOUND (err u103))
(define-constant ERR_NOT_MEMBER (err u105))
(define-constant ERR_INVALID_SCORE (err u200))
(define-constant ERR_INVALID_PERIOD (err u201))

;; Reputation tracking for pool members
(define-map member-reputation
  { pool-id: uint, member: principal }
  {
    trust-score: uint,           ;; 0-100 trust rating
    payment-streak: uint,        ;; consecutive on-time payments
    total-contributions: uint,   ;; lifetime contribution amount
    late-payments: uint,         ;; count of late payments
    participation-days: uint,    ;; days active in pool
    last-activity: uint,         ;; last block of activity
    reputation-tier: uint        ;; 0=bronze, 1=silver, 2=gold, 3=platinum
  }
)

;; Payment behavior tracking
(define-map payment-history
  { pool-id: uint, member: principal, period: uint }
  {
    expected-amount: uint,
    paid-amount: uint,
    payment-block: uint,
    due-block: uint,
    on-time: bool,
    partial-payment: bool
  }
)

;; Pool reputation thresholds
(define-map reputation-tiers
  { pool-id: uint, tier: uint }
  {
    min-trust-score: uint,
    min-payment-streak: uint,
    min-contributions: uint,
    tier-benefits: (string-ascii 100)
  }
)

;; Initialize reputation for new member
(define-public (initialize-member-reputation (pool-id uint) (member principal))
  (let
    (
      (pool-exists (is-some (contract-call? .Billvault get-pool pool-id)))
      (member-exists (is-some (contract-call? .Billvault get-member-info pool-id member)))
    )
    (asserts! pool-exists ERR_POOL_NOT_FOUND)
    (asserts! member-exists ERR_NOT_MEMBER)
    (map-set member-reputation
      { pool-id: pool-id, member: member }
      {
        trust-score: u75,        ;; start with neutral score
        payment-streak: u0,
        total-contributions: u0,
        late-payments: u0,
        participation-days: u0,
        last-activity: stacks-block-height,
        reputation-tier: u0      ;; bronze tier
      }
    )
    (ok true)
  )
)

;; Record payment behavior
(define-public (record-payment (pool-id uint) (member principal) (period uint) (expected-amount uint) (paid-amount uint) (due-block uint))
  (let
    (
      (pool-data (unwrap! (contract-call? .Billvault get-pool pool-id) ERR_POOL_NOT_FOUND))
      (current-rep (default-to
        { trust-score: u75, payment-streak: u0, total-contributions: u0, late-payments: u0, participation-days: u0, last-activity: u0, reputation-tier: u0 }
        (map-get? member-reputation { pool-id: pool-id, member: member })
      ))
      (on-time (< stacks-block-height due-block))
      (full-payment (>= paid-amount expected-amount))
    )
    (asserts! (is-eq tx-sender (get admin pool-data)) ERR_UNAUTHORIZED)
    
    ;; Record payment history
    (map-set payment-history
      { pool-id: pool-id, member: member, period: period }
      {
        expected-amount: expected-amount,
        paid-amount: paid-amount,
        payment-block: stacks-block-height,
        due-block: due-block,
        on-time: on-time,
        partial-payment: (not full-payment)
      }
    )
    
    ;; Update reputation
    (let
      (
        (new-streak (if on-time (+ (get payment-streak current-rep) u1) u0))
        (new-late-payments (if (not on-time) (+ (get late-payments current-rep) u1) (get late-payments current-rep)))
        (new-contributions (+ (get total-contributions current-rep) paid-amount))
        (new-trust-score (calculate-trust-score current-rep on-time full-payment))
      )
      (map-set member-reputation
        { pool-id: pool-id, member: member }
        (merge current-rep {
          payment-streak: new-streak,
          late-payments: new-late-payments,
          total-contributions: new-contributions,
          trust-score: new-trust-score,
          last-activity: stacks-block-height,
          reputation-tier: (determine-tier new-trust-score new-streak new-contributions)
        })
      )
      (ok new-trust-score)
    )
  )
)

;; Update member activity
(define-public (update-activity (pool-id uint) (member principal))
  (let
    (
      (current-rep (unwrap! (map-get? member-reputation { pool-id: pool-id, member: member }) ERR_NOT_MEMBER))
      (days-since-join (/ (- stacks-block-height (get last-activity current-rep)) u144)) ;; ~144 blocks per day
    )
    (map-set member-reputation
      { pool-id: pool-id, member: member }
      (merge current-rep {
        participation-days: (+ (get participation-days current-rep) days-since-join),
        last-activity: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Set reputation tier thresholds for pool
(define-public (set-tier-thresholds (pool-id uint) (tier uint) (min-trust uint) (min-streak uint) (min-contrib uint) (benefits (string-ascii 100)))
  (let
    (
      (pool-data (unwrap! (contract-call? .Billvault get-pool pool-id) ERR_POOL_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get admin pool-data)) ERR_UNAUTHORIZED)
    (asserts! (<= tier u3) ERR_INVALID_SCORE)
    (asserts! (<= min-trust u100) ERR_INVALID_SCORE)
    (map-set reputation-tiers
      { pool-id: pool-id, tier: tier }
      {
        min-trust-score: min-trust,
        min-payment-streak: min-streak,
        min-contributions: min-contrib,
        tier-benefits: benefits
      }
    )
    (ok true)
  )
)

;; Calculate trust score based on payment behavior
(define-private (calculate-trust-score (current-rep { trust-score: uint, payment-streak: uint, total-contributions: uint, late-payments: uint, participation-days: uint, last-activity: uint, reputation-tier: uint }) (on-time bool) (full-payment bool))
  (let
    (
      (current-score (get trust-score current-rep))
      (streak-bonus (if (and on-time full-payment) u2 u0))
      (late-penalty (if (not on-time) u3 u0))
      (partial-penalty (if (not full-payment) u1 u0))
    )
    (let
      (
        (adjusted-score (+ current-score streak-bonus))
        (final-score (if (> (+ late-penalty partial-penalty) adjusted-score)
                        u0
                        (- adjusted-score (+ late-penalty partial-penalty))))
      )
      (if (> final-score u100) u100 final-score)
    )
  )
)

;; Determine reputation tier
(define-private (determine-tier (trust-score uint) (payment-streak uint) (total-contributions uint))
  (if (and (>= trust-score u90) (>= payment-streak u10) (>= total-contributions u10000000)) u3  ;; platinum
    (if (and (>= trust-score u80) (>= payment-streak u5) (>= total-contributions u5000000)) u2   ;; gold
      (if (and (>= trust-score u70) (>= payment-streak u3) (>= total-contributions u1000000)) u1 ;; silver
        u0)))                                                                                      ;; bronze
)

;; Get member reputation
(define-read-only (get-member-reputation (pool-id uint) (member principal))
  (map-get? member-reputation { pool-id: pool-id, member: member })
)

;; Get payment history for specific period
(define-read-only (get-payment-history (pool-id uint) (member principal) (period uint))
  (map-get? payment-history { pool-id: pool-id, member: member, period: period })
)

;; Get tier thresholds
(define-read-only (get-tier-thresholds (pool-id uint) (tier uint))
  (map-get? reputation-tiers { pool-id: pool-id, tier: tier })
)

;; Check if member qualifies for tier
(define-read-only (qualifies-for-tier (pool-id uint) (member principal) (target-tier uint))
  (match (map-get? member-reputation { pool-id: pool-id, member: member })
    rep-data
      (match (map-get? reputation-tiers { pool-id: pool-id, tier: target-tier })
        tier-data
          (and
            (>= (get trust-score rep-data) (get min-trust-score tier-data))
            (>= (get payment-streak rep-data) (get min-payment-streak tier-data))
            (>= (get total-contributions rep-data) (get min-contributions tier-data))
          )
        false
      )
    false
  )
)

;; Get tier name as string
(define-read-only (get-tier-name (tier uint))
  (if (is-eq tier u3) "Platinum"
    (if (is-eq tier u2) "Gold"
      (if (is-eq tier u1) "Silver"
        "Bronze")))
)
