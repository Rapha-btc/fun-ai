;; Faktory.Fun 
;; @version 1.0
(impl-trait .faktory-dex-trait-v1.dex-trait)
(use-trait faktory-token .faktory-trait-v1.sip-010-trait) ;; 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE

(define-constant ERR-MARKET-CLOSED (err u1001))
(define-constant ERR-STX-NON-POSITIVE (err u1002))
(define-constant ERR-STX-BALANCE-TOO-LOW (err u1003))
(define-constant ERR-FT-NON-POSITIVE (err u1004))
(define-constant ERR_NATIVE_FAILURE (err u99))
(define-constant ERR-TOKEN-NOT-AUTH (err u401))
(define-constant ERR-DIRECT-CALL-REQUIRED (err u402))

(define-constant THIS-CONTRACT (as-contract tx-sender))
(define-constant FEE-RECEIVER 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5) ;; SPZYMEK0JYQMPGRTHM0TAMF0J2P70ZX4BHQZNHA8
(define-constant G-RECEIVER 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG) ;; SP3MZAPFZEA9MZHBCGHG3TYKNFGDMNF5FW9RZ3AXS
(define-constant AMM-RECEIVER 'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC) ;; SP2BN9JN4WEG02QYVX5Y21VMB2JWV3W0KNHPH9R4P
(define-constant CANT-BE-EVIL 'ST2NEB84ASENDXKYGJPQW86YXQCEFEX2ZQPG87ND) ;; SP000000000000000000002Q6VF78
(define-constant DEV tx-sender)
(define-constant DEX-TOKEN .name-faktory) ;; 'SP29D6YMDNAKN1P045T6Z817RTE1AC0JAA99WAX2B.fak-dot-fun-faktory

;; token constants
(define-constant TARGET_STX u6000000000)
(define-constant FAK_STX u1200000000)
(define-constant GRAD-FEE u120000000)

;; data vars
(define-data-var open bool false)
(define-data-var fak-ustx uint u0)
(define-data-var ft-balance uint u0)
(define-data-var stx-balance uint u0)
(define-data-var burn-rate uint u20)
(define-data-var dev-premium uint u10)

(define-public (buy (ft <faktory-token>) (ustx uint))
  (begin
    (asserts! (is-eq DEX-TOKEN (contract-of ft)) ERR-TOKEN-NOT-AUTH)
    (asserts! (var-get open) ERR-MARKET-CLOSED)
    (asserts! (> ustx u0) ERR-STX-NON-POSITIVE)
    (let ((total-stx (var-get stx-balance))
          (total-stk (+ total-stx (var-get fak-ustx)))
          (total-ft (var-get ft-balance))
          (k (* total-ft total-stk))
          (fee (/ (* ustx u2) u100))
          (stx-in (- ustx fee))
          (new-stk (+ total-stk stx-in))
          (new-ft (/ k new-stk))
          (tokens-out (- total-ft new-ft))
          (new-stx (+ total-stx stx-in))
          (ft-receiver tx-sender))
      (try! (stx-transfer? fee tx-sender FEE-RECEIVER))
      (try! (stx-transfer? stx-in tx-sender (as-contract tx-sender)))
      (try! (as-contract (contract-call? ft transfer tokens-out tx-sender ft-receiver none)))
      (if (>= new-stx TARGET_STX)
        (begin
          (let ((grad-amount (/ (* new-ft (var-get burn-rate)) u100)) ;; grad = new * 20%
                (dev-amount (/ (* grad-amount (var-get dev-premium)) u100)) ;; no need this
                (burn-amount (- grad-amount dev-amount)) ;; no need this
                (amm-amount (- new-ft grad-amount))
                (amm-ustx (- new-stx GRAD-FEE)))
            (try! (as-contract (contract-call? ft transfer burn-amount tx-sender CANT-BE-EVIL none)))
            (try! (as-contract (contract-call? ft transfer dev-amount tx-sender DEV none))) ;; no 
            (try! (as-contract (contract-call? ft transfer amm-amount tx-sender AMM-RECEIVER none)))
            (try! (as-contract (stx-transfer? amm-ustx tx-sender AMM-RECEIVER)))
            (try! (as-contract (stx-transfer? GRAD-FEE tx-sender G-RECEIVER)))
            (var-set open false)
            (var-set stx-balance u0)
            (var-set ft-balance u0)
            (print {tokens-out: tokens-out, burn-amount: burn-amount, amm-amount: amm-amount,
                    amm-ustx: amm-ustx,
                    stx-balance: u0, ft-balance: u0,
                    fee: fee, grad-fee: GRAD-FEE,
                    open: false})
            (ok true)))
        (begin
          (var-set stx-balance new-stx)
          (var-set ft-balance new-ft)
          (print {tokens-out: tokens-out,
                  stx-balance: new-stx, ft-balance: new-ft,
                  fee: fee,
                  open: true})
          (ok true))))))

(define-read-only (get-in (ustx uint))
  (let ((total-stx (var-get stx-balance))
        (total-stk (+ total-stx (var-get fak-ustx)))
        (total-ft (var-get ft-balance))
        (k (* total-ft total-stk))
        (fee (/ (* ustx u2) u100))
        (stx-in (- ustx fee))
        (new-stk (+ total-stk stx-in))
        (new-ft (/ k new-stk))
        (tokens-out (- total-ft new-ft))
        (raw-to-grad (- TARGET_STX total-stx))
        (stx-to-grad (/ (* raw-to-grad u103) u100)))
    (ok {stx-in: stx-in,
         fee: fee,
         tokens-out: tokens-out,
         ft-balance: total-ft,
         new-ft: new-ft,
         total-stx: total-stx,
         new-stx: (+ total-stx stx-in),
         stx-to-grad: stx-to-grad})))

(define-public (sell (ft <faktory-token>) (amount uint))
  (begin
    (asserts! (is-eq DEX-TOKEN (contract-of ft)) ERR-TOKEN-NOT-AUTH)
    (asserts! (is-eq contract-caller tx-sender) ERR-DIRECT-CALL-REQUIRED)
    (asserts! (var-get open) ERR-MARKET-CLOSED)
    (asserts! (> amount u0) ERR-FT-NON-POSITIVE)
    (let ((total-stx (var-get stx-balance))
          (total-stk (+ total-stx (var-get fak-ustx)))
          (total-ft (var-get ft-balance))
          (k (* total-ft total-stk))
          (new-ft (+ total-ft amount))
          (new-stk (/ k new-ft))
          (stx-out (- (- total-stk new-stk) u1))
          (fee (/ (* stx-out u2) u100))
          (stx-to-receiver (- stx-out fee))
          (new-stx (- total-stx stx-out))
          (stx-receiver tx-sender))
      (asserts! (>= total-stx stx-out) ERR-STX-BALANCE-TOO-LOW)
      (try! (contract-call? ft transfer amount tx-sender THIS-CONTRACT none))
      (try! (as-contract (stx-transfer? stx-to-receiver tx-sender stx-receiver)))
      (try! (as-contract (stx-transfer? fee tx-sender FEE-RECEIVER)))
      (var-set stx-balance new-stx)
      (var-set ft-balance new-ft)
      (print {stx-to-receiver: stx-to-receiver,
              stx-balance: new-stx, ft-balance: new-ft,
              fee: fee,
              open: true})
      (ok true))))

(define-read-only (get-out (amount uint))
  (let ((total-stx (var-get stx-balance))
        (total-stk (+ total-stx (var-get fak-ustx)))
        (total-ft (var-get ft-balance))
        (k (* total-ft total-stk))
        (new-ft (+ total-ft amount))
        (new-stk (/ k new-ft))
        (stx-out (- (- total-stk new-stk) u1))
        (fee (/ (* stx-out u2) u100))
        (stx-to-receiver (- stx-out fee)))
    (ok {amount-in: amount,
         stx-out: stx-out,
         fee: fee,
         stx-to-receiver: stx-to-receiver,
         total-stx: total-stx,
         new-stx: (- total-stx stx-out),
         ft-balance: total-ft,
         new-ft: new-ft})))

(define-read-only (get-open)
  (ok (var-get open)))

;; boot dex
(begin
  (var-set fak-ustx FAK_STX)
  (var-set ft-balance u68961687931993)
  (var-set stx-balance u666667)
  (var-set open true)
  (var-set burn-rate u20)
  (var-set dev-premium u10)
  (try! (stx-transfer? u1000000 tx-sender 'ST2REHHS5J3CERCRBEPMGH7921Q6PYKAADT7JP2VB)) ;;SP2Z2N5HGEK6KD2MBBKN0WH7MS1W70AYXBPBG0N2F
  (print { 
        type: "faktory-dex-trait-v1", 
        dexContract: (as-contract tx-sender),
        ammReceiver: 'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC,
        ;; hash: "363acbe80e3698b90cd0500fd8c64c56ec3d2caa674483aeb86d8772e8cf6fe3"  ;; do we need a hash of that contract?
   })
  (ok true))