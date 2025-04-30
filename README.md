# FlexNest

A flexible blockchain-based subscription service manager for tracking and optimizing recurring costs

This project is built with Clarity smart contracts for the Stacks blockchain.

## Overview

FlexNest provides a comprehensive subscription management platform that enables users to securely track, manage, and optimize their recurring subscription costs. The platform leverages blockchain technology to ensure transparent and immutable record-keeping while providing powerful analytics and automation features.

## Core Features

- Subscription registration and management
- Automated payment processing and scheduling
- Spending analytics and optimization recommendations
- Multi-token payment support (STX and other fungible tokens)
- Usage tracking and insights
- Budget management and spending controls

## Architecture

The platform consists of three main smart contracts that work together:

### 1. Subscription Manager (`subscription-manager`)
- Core contract for managing subscription records
- Handles subscription registration, updates, and status management
- Maintains comprehensive subscription history
- Supports multiple billing cycles (monthly, quarterly, biannual, annual)

### 2. Payment Processor (`payment-processor`)
- Handles all payment-related operations
- Supports both manual and automated payments
- Implements payment thresholds and approval workflows
- Manages payment history and service registrations
- Supports multiple payment tokens

### 3. Analytics Engine (`subscription-analytics`)
- Provides spending analysis and insights
- Generates optimization recommendations
- Tracks usage patterns and spending trends
- Manages category-based budgeting
- Identifies opportunities for cost savings

## Smart Contract Functions

### Subscription Management
```clarity
;; Register a new subscription
(register-subscription 
  (service-name (string-ascii 100))
  (payment-amount uint)
  (billing-cycle uint)
  (next-payment-date uint))

;; Update subscription details
(update-subscription
  (subscription-id uint)
  (service-name (string-ascii 100))
  (payment-amount uint)
  (billing-cycle uint)
  (next-payment-date uint))

;; Change subscription status
(update-subscription-status
  (subscription-id uint)
  (new-status uint))
```

### Payment Processing
```clarity
;; Process a manual payment
(make-payment (subscription-id uint))

;; Configure auto-payment settings
(configure-auto-payment 
  (enabled bool)
  (max-payment-threshold uint)
  (requires-approval-above-threshold bool))

;; Process automated payment
(process-auto-payment (subscription-id uint))
```

### Analytics and Optimization
```clarity
;; Get monthly spending analysis
(get-monthly-spending (user principal))

;; Get spending by category
(get-spending-by-category 
  (user principal)
  (category (string-ascii 20)))

;; Generate optimization suggestions
(generate-optimization-suggestions)
```

## Getting Started

1. Deploy the smart contracts in the following order:
   - `subscription-manager`
   - `payment-processor`
   - `subscription-analytics`

2. Initialize auto-payment settings:
```clarity
(contract-call? payment-processor configure-auto-payment true u1000000 true)
```

3. Register a subscription:
```clarity
(contract-call? subscription-manager register-subscription 
  "Netflix"
  u1499000
  u30
  block-height)
```

## Security Considerations

- All contracts implement proper authorization checks
- Payment thresholds and approval workflows prevent unauthorized spending
- Historical records are maintained for auditing
- Status changes are tracked and verified
- Multi-step processes for critical operations

## Future Enhancements

- Integration with recommendation engines
- Advanced spending optimization algorithms
- Additional payment token support
- Enhanced analytics features
- Social features for subscription sharing

## License

This project is licensed under the MIT License - see the LICENSE file for details.