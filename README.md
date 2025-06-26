# 💰 Billvault - Utility Bill Escrow System

A community-pooled billing system with usage oracles built on Stacks blockchain using Clarity smart contracts.

## 🌟 Overview

Billvault enables communities to pool resources for utility bills, with fair cost distribution based on actual usage data provided by authorized oracles. Perfect for shared housing, co-working spaces, or community facilities.

## ✨ Features

- 🏠 **Community Pools**: Create and manage utility bill pools
- 👥 **Member Management**: Join pools and contribute funds
- 📊 **Usage Tracking**: Oracle-verified usage data for fair billing
- 💸 **Automated Payments**: Secure escrow and bill payment system
- ⚖️ **Fair Distribution**: Costs distributed based on actual usage
- 🔒 **Secure Escrow**: Funds held safely until bills are due

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Run `clarinet check` to verify the contract

## 📖 Usage Guide

### Creating a Pool

```clarity
(contract-call? .billvault create-pool "Apartment Electric" u10 "electricity" 'SP1234...ORACLE)
```

### Joining a Pool

```clarity
(contract-call? .billvault join-pool u1)
```

### Contributing Funds

```clarity
(contract-call? .billvault contribute-to-pool u1 u1000000)
```

### Submitting Bills (Pool Admin)

```clarity
(contract-call? .billvault submit-bill u1 u500000 u1000)
```

### Oracle Usage Data Submission

```clarity
(contract-call? .billvault submit-usage-data u1 'SP1234...MEMBER u202401 u150)
```

### Paying Bills

```clarity
(contract-call? .billvault pay-bill u1)
```

## 🔍 Read-Only Functions

- `get-pool`: Retrieve pool information
- `get-member-info`: Get member details
- `get-member-balance`: Check member balance
- `get-bill`: Retrieve bill information
- `get-usage-data`: Get usage data for a member
- `is-oracle-authorized`: Check oracle authorization status

## 🛡️ Security Features

- **Access Control**: Pool admins control bill submission and payment
- **Oracle Authorization**: Only authorized oracles can submit usage data
- **Balance Verification**: Ensures sufficient funds before payments
- **Member Validation**: Prevents unauthorized access to pool functions

## 🏗️ Contract Architecture

### Data Structures

- **Pools**: Store pool metadata and configuration
- **Members**: Track member participation and contributions
- **Bills**: Manage bill lifecycle and payment status
- **Usage Data**: Oracle-verified consumption data
- **Balances**: Individual member fund tracking

### Key Constants

- `ERR_UNAUTHORIZED`: Access denied
- `ERR_INSUFFICIENT_BALANCE`: Not enough funds
- `ERR_POOL_NOT_FOUND`: Invalid pool ID
- `ERR_NOT_MEMBER`: User not in pool

## 🧪 Testing

Run the test suite:

```bash
clarinet test
```


