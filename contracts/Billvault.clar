(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_POOL_NOT_FOUND (err u103))
(define-constant ERR_ALREADY_MEMBER (err u104))
(define-constant ERR_NOT_MEMBER (err u105))
(define-constant ERR_BILL_NOT_FOUND (err u106))
(define-constant ERR_PAYMENT_FAILED (err u107))
(define-constant ERR_ORACLE_UNAUTHORIZED (err u108))
(define-constant ERR_INVALID_USAGE (err u109))
(define-constant ERR_POOL_FULL (err u110))
(define-constant ERR_SCHEDULE_NOT_FOUND (err u111))
(define-constant ERR_SCHEDULE_INACTIVE (err u112))
(define-constant ERR_SCHEDULE_NOT_DUE (err u113))
(define-constant ERR_INVALID_FREQUENCY (err u114))
(define-constant ERR_SCHEDULE_EXISTS (err u115))

(define-data-var next-pool-id uint u1)
(define-data-var next-bill-id uint u1)
(define-data-var next-schedule-id uint u1)

(define-map pools
  { pool-id: uint }
  {
    name: (string-ascii 50),
    admin: principal,
    max-members: uint,
    current-members: uint,
    total-balance: uint,
    utility-type: (string-ascii 20),
    oracle: principal,
    created-at: uint
  }
)

(define-map pool-members
  { pool-id: uint, member: principal }
  {
    contribution: uint,
    usage-share: uint,
    joined-at: uint,
    active: bool
  }
)

(define-map bills
  { bill-id: uint }
  {
    pool-id: uint,
    amount: uint,
    due-date: uint,
    paid: bool,
    oracle-verified: bool,
    created-at: uint
  }
)

(define-map member-balances
  { pool-id: uint, member: principal }
  { balance: uint }
)

(define-map usage-data
  { pool-id: uint, member: principal, period: uint }
  { usage-amount: uint }
)

(define-map authorized-oracles
  { oracle: principal }
  { authorized: bool }
)

(define-map payment-schedules
  { schedule-id: uint }
  {
    pool-id: uint,
    frequency-blocks: uint,
    expected-amount: uint,
    next-payment-block: uint,
    active: bool,
    created-at: uint,
    last-executed: uint,
    auto-execute: bool
  }
)

(define-map schedule-history
  { schedule-id: uint, execution-id: uint }
  {
    executed-at: uint,
    amount-paid: uint,
    bill-id: uint,
    success: bool
  }
)

(define-public (create-pool (name (string-ascii 50)) (max-members uint) (utility-type (string-ascii 20)) (oracle principal))
  (let
    (
      (pool-id (var-get next-pool-id))
    )
    (asserts! (> max-members u0) ERR_INVALID_AMOUNT)
    (asserts! (<= max-members u50) ERR_INVALID_AMOUNT)
    (map-set pools
      { pool-id: pool-id }
      {
        name: name,
        admin: tx-sender,
        max-members: max-members,
        current-members: u0,
        total-balance: u0,
        utility-type: utility-type,
        oracle: oracle,
        created-at: stacks-block-height
      }
    )
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

(define-public (join-pool (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
      (current-members (get current-members pool))
      (max-members (get max-members pool))
    )
    (asserts! (is-none (map-get? pool-members { pool-id: pool-id, member: tx-sender })) ERR_ALREADY_MEMBER)
    (asserts! (< current-members max-members) ERR_POOL_FULL)
    (map-set pool-members
      { pool-id: pool-id, member: tx-sender }
      {
        contribution: u0,
        usage-share: u0,
        joined-at: stacks-block-height,
        active: true
      }
    )
    (map-set member-balances
      { pool-id: pool-id, member: tx-sender }
      { balance: u0 }
    )
    (map-set pools
      { pool-id: pool-id }
      (merge pool { current-members: (+ current-members u1) })
    )
    (ok true)
  )
)

(define-public (contribute-to-pool (pool-id uint) (amount uint))
  (let
    (
      (pool (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
      (member-data (unwrap! (map-get? pool-members { pool-id: pool-id, member: tx-sender }) ERR_NOT_MEMBER))
      (current-balance (default-to { balance: u0 } (map-get? member-balances { pool-id: pool-id, member: tx-sender })))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set member-balances
      { pool-id: pool-id, member: tx-sender }
      { balance: (+ (get balance current-balance) amount) }
    )
    (map-set pool-members
      { pool-id: pool-id, member: tx-sender }
      (merge member-data { contribution: (+ (get contribution member-data) amount) })
    )
    (map-set pools
      { pool-id: pool-id }
      (merge pool { total-balance: (+ (get total-balance pool) amount) })
    )
    (ok true)
  )
)

(define-public (submit-bill (pool-id uint) (amount uint) (due-date uint))
  (let
    (
      (pool (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
      (bill-id (var-get next-bill-id))
    )
    (asserts! (is-eq tx-sender (get admin pool)) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> due-date stacks-block-height) ERR_INVALID_AMOUNT)
    (map-set bills
      { bill-id: bill-id }
      {
        pool-id: pool-id,
        amount: amount,
        due-date: due-date,
        paid: false,
        oracle-verified: false,
        created-at: stacks-block-height
      }
    )
    (var-set next-bill-id (+ bill-id u1))
    (ok bill-id)
  )
)

(define-public (authorize-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-oracles { oracle: oracle } { authorized: true })
    (ok true)
  )
)

(define-public (submit-usage-data (pool-id uint) (member principal) (period uint) (usage-amount uint))
  (let
    (
      (pool (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get oracle pool)) ERR_ORACLE_UNAUTHORIZED)
    (asserts! (default-to false (get authorized (map-get? authorized-oracles { oracle: tx-sender }))) ERR_ORACLE_UNAUTHORIZED)
    (asserts! (> usage-amount u0) ERR_INVALID_USAGE)
    (map-set usage-data
      { pool-id: pool-id, member: member, period: period }
      { usage-amount: usage-amount }
    )
    (ok true)
  )
)

(define-public (pay-bill (bill-id uint))
  (let
    (
      (bill (unwrap! (map-get? bills { bill-id: bill-id }) ERR_BILL_NOT_FOUND))
      (pool (unwrap! (map-get? pools { pool-id: (get pool-id bill) }) ERR_POOL_NOT_FOUND))
      (total-balance (get total-balance pool))
      (bill-amount (get amount bill))
    )
    (asserts! (is-eq tx-sender (get admin pool)) ERR_UNAUTHORIZED)
    (asserts! (not (get paid bill)) ERR_PAYMENT_FAILED)
    (asserts! (>= total-balance bill-amount) ERR_INSUFFICIENT_BALANCE)
    (try! (as-contract (stx-transfer? bill-amount tx-sender (get admin pool))))
    (map-set bills
      { bill-id: bill-id }
      (merge bill { paid: true })
    )
    (map-set pools
      { pool-id: (get pool-id bill) }
      (merge pool { total-balance: (- total-balance bill-amount) })
    )
    (try! (distribute-bill-cost bill-id))
    (ok true)
  )
)

(define-private (distribute-bill-cost (bill-id uint))
  (let
    (
      (bill (unwrap! (map-get? bills { bill-id: bill-id }) ERR_BILL_NOT_FOUND))
      (pool (unwrap! (map-get? pools { pool-id: (get pool-id bill) }) ERR_POOL_NOT_FOUND))
      (bill-amount (get amount bill))
      (member-count (get current-members pool))
    )
    (if (> member-count u0)
      (let ((cost-per-member (/ bill-amount member-count)))
        (ok true))
      (ok true))
  )
)

(define-public (withdraw-excess (pool-id uint) (amount uint))
  (let
    (
      (pool (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
      (member-balance (default-to { balance: u0 } (map-get? member-balances { pool-id: pool-id, member: tx-sender })))
      (current-balance (get balance member-balance))
    )
    (asserts! (is-some (map-get? pool-members { pool-id: pool-id, member: tx-sender })) ERR_NOT_MEMBER)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set member-balances
      { pool-id: pool-id, member: tx-sender }
      { balance: (- current-balance amount) }
    )
    (ok true)
  )
)

(define-public (create-payment-schedule (pool-id uint) (frequency-blocks uint) (expected-amount uint) (auto-execute bool))
  (let
    (
      (pool (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
      (schedule-id (var-get next-schedule-id))
    )
    (asserts! (is-eq tx-sender (get admin pool)) ERR_UNAUTHORIZED)
    (asserts! (> frequency-blocks u0) ERR_INVALID_FREQUENCY)
    (asserts! (> expected-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (get-active-schedule-for-pool pool-id)) ERR_SCHEDULE_EXISTS)
    (map-set payment-schedules
      { schedule-id: schedule-id }
      {
        pool-id: pool-id,
        frequency-blocks: frequency-blocks,
        expected-amount: expected-amount,
        next-payment-block: (+ stacks-block-height frequency-blocks),
        active: true,
        created-at: stacks-block-height,
        last-executed: u0,
        auto-execute: auto-execute
      }
    )
    (var-set next-schedule-id (+ schedule-id u1))
    (ok schedule-id)
  )
)

(define-public (execute-scheduled-payment (schedule-id uint))
  (let
    (
      (schedule (unwrap! (map-get? payment-schedules { schedule-id: schedule-id }) ERR_SCHEDULE_NOT_FOUND))
      (pool (unwrap! (map-get? pools { pool-id: (get pool-id schedule) }) ERR_POOL_NOT_FOUND))
      (bill-id (var-get next-bill-id))
    )
    (asserts! (get active schedule) ERR_SCHEDULE_INACTIVE)
    (asserts! (>= stacks-block-height (get next-payment-block schedule)) ERR_SCHEDULE_NOT_DUE)
    (asserts! (>= (get total-balance pool) (get expected-amount schedule)) ERR_INSUFFICIENT_BALANCE)
    (try! (create-and-pay-scheduled-bill schedule-id bill-id))
    (map-set payment-schedules
      { schedule-id: schedule-id }
      (merge schedule 
        {
          next-payment-block: (+ stacks-block-height (get frequency-blocks schedule)),
          last-executed: stacks-block-height
        }
      )
    )
    (ok bill-id)
  )
)

(define-public (toggle-schedule-status (schedule-id uint))
  (let
    (
      (schedule (unwrap! (map-get? payment-schedules { schedule-id: schedule-id }) ERR_SCHEDULE_NOT_FOUND))
      (pool (unwrap! (map-get? pools { pool-id: (get pool-id schedule) }) ERR_POOL_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get admin pool)) ERR_UNAUTHORIZED)
    (map-set payment-schedules
      { schedule-id: schedule-id }
      (merge schedule { active: (not (get active schedule)) })
    )
    (ok (not (get active schedule)))
  )
)

(define-public (update-schedule-amount (schedule-id uint) (new-amount uint))
  (let
    (
      (schedule (unwrap! (map-get? payment-schedules { schedule-id: schedule-id }) ERR_SCHEDULE_NOT_FOUND))
      (pool (unwrap! (map-get? pools { pool-id: (get pool-id schedule) }) ERR_POOL_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get admin pool)) ERR_UNAUTHORIZED)
    (asserts! (> new-amount u0) ERR_INVALID_AMOUNT)
    (map-set payment-schedules
      { schedule-id: schedule-id }
      (merge schedule { expected-amount: new-amount })
    )
    (ok true)
  )
)

(define-public (update-schedule-frequency (schedule-id uint) (new-frequency uint))
  (let
    (
      (schedule (unwrap! (map-get? payment-schedules { schedule-id: schedule-id }) ERR_SCHEDULE_NOT_FOUND))
      (pool (unwrap! (map-get? pools { pool-id: (get pool-id schedule) }) ERR_POOL_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get admin pool)) ERR_UNAUTHORIZED)
    (asserts! (> new-frequency u0) ERR_INVALID_FREQUENCY)
    (map-set payment-schedules
      { schedule-id: schedule-id }
      (merge schedule 
        { 
          frequency-blocks: new-frequency,
          next-payment-block: (+ stacks-block-height new-frequency)
        }
      )
    )
    (ok true)
  )
)

(define-public (trigger-auto-payments)
  (let
    (
      (current-block stacks-block-height)
    )
    (ok (process-due-schedules current-block))
  )
)

(define-private (create-and-pay-scheduled-bill (schedule-id uint) (bill-id uint))
  (let
    (
      (schedule (unwrap! (map-get? payment-schedules { schedule-id: schedule-id }) ERR_SCHEDULE_NOT_FOUND))
      (pool (unwrap! (map-get? pools { pool-id: (get pool-id schedule) }) ERR_POOL_NOT_FOUND))
      (expected-amount (get expected-amount schedule))
    )
    (map-set bills
      { bill-id: bill-id }
      {
        pool-id: (get pool-id schedule),
        amount: expected-amount,
        due-date: stacks-block-height,
        paid: false,
        oracle-verified: true,
        created-at: stacks-block-height
      }
    )
    (var-set next-bill-id (+ bill-id u1))
    (try! (as-contract (stx-transfer? expected-amount tx-sender (get admin pool))))
    (map-set bills
      { bill-id: bill-id }
      {
        pool-id: (get pool-id schedule),
        amount: expected-amount,
        due-date: stacks-block-height,
        paid: true,
        oracle-verified: true,
        created-at: stacks-block-height
      }
    )
    (map-set pools
      { pool-id: (get pool-id schedule) }
      (merge pool { total-balance: (- (get total-balance pool) expected-amount) })
    )
    (try! (distribute-bill-cost bill-id))
    (let ((log-result (log-schedule-execution schedule-id bill-id expected-amount true)))
      (ok true))
  )
)

(define-private (process-due-schedules (current-block uint))
  (begin
    (ok true)
  )
)

(define-private (log-schedule-execution (schedule-id uint) (bill-id uint) (amount uint) (success bool))
  (let
    (
      (execution-id (+ (default-to u0 (get-last-execution-id schedule-id)) u1))
    )
    (map-set schedule-history
      { schedule-id: schedule-id, execution-id: execution-id }
      {
        executed-at: stacks-block-height,
        amount-paid: amount,
        bill-id: bill-id,
        success: success
      }
    )
    (ok execution-id)
  )
)

(define-private (get-active-schedule-for-pool (pool-id uint))
  none
)

(define-private (get-last-execution-id (schedule-id uint))
  (some u0)
)

(define-read-only (get-pool (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

(define-read-only (get-member-info (pool-id uint) (member principal))
  (map-get? pool-members { pool-id: pool-id, member: member })
)

(define-read-only (get-member-balance (pool-id uint) (member principal))
  (map-get? member-balances { pool-id: pool-id, member: member })
)

(define-read-only (get-bill (bill-id uint))
  (map-get? bills { bill-id: bill-id })
)

(define-read-only (get-usage-data (pool-id uint) (member principal) (period uint))
  (map-get? usage-data { pool-id: pool-id, member: member, period: period })
)

(define-read-only (is-oracle-authorized (oracle principal))
  (default-to false (get authorized (map-get? authorized-oracles { oracle: oracle })))
)

(define-read-only (get-pool-count)
  (- (var-get next-pool-id) u1)
)

(define-read-only (get-bill-count)
  (- (var-get next-bill-id) u1)
)

(define-read-only (get-payment-schedule (schedule-id uint))
  (map-get? payment-schedules { schedule-id: schedule-id })
)

(define-read-only (get-schedule-history (schedule-id uint) (execution-id uint))
  (map-get? schedule-history { schedule-id: schedule-id, execution-id: execution-id })
)

(define-read-only (get-schedule-count)
  (- (var-get next-schedule-id) u1)
)

(define-read-only (is-schedule-due (schedule-id uint))
  (match (map-get? payment-schedules { schedule-id: schedule-id })
    schedule (and (get active schedule) (>= stacks-block-height (get next-payment-block schedule)))
    false
  )
)

(define-read-only (get-next-payment-block (schedule-id uint))
  (match (map-get? payment-schedules { schedule-id: schedule-id })
    schedule (some (get next-payment-block schedule))
    none
  )
)

(define-read-only (estimate-schedule-cost (schedule-id uint))
  (match (map-get? payment-schedules { schedule-id: schedule-id })
    schedule (some (get expected-amount schedule))
    none
  )
)