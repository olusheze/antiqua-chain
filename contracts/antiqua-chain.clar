;; Antiqua Chain: Historical Item Verification Framework
;; A decentralized system for tracking, authenticating, and managing historical artifacts
;; and their ownership history across generations using blockchain technology.

;; =========================================================
;; Section 1: Protocol Governance Structure
;; =========================================================

;; The protocol governor - set to the contract deployer
(define-constant protocol-governor tx-sender)

;; Tracks the total number of documented artifacts in the system
(define-data-var artifact-counter uint u0)
;; =========================================================
;; Section 2: Error Code Definitions and Constants
;; =========================================================

;; Error codes for various operations
(define-constant error-not-found (err u410))
(define-constant error-duplicate-record (err u411))
(define-constant error-invalid-artifact-title (err u412))
(define-constant error-supervisor-only (err u413))
(define-constant error-access-denied (err u414))
(define-constant error-invalid-metadata-format (err u415))
(define-constant error-insufficient-artifact-size (err u416))
(define-constant error-unauthorized-action (err u417))
(define-constant error-invalid-data-format (err u418))

;; =========================================================
;; Section 3: Data Storage Structures
;; =========================================================

;; Primary artifact documentation storage
(define-map artifact-archive
  { artifact-id: uint }
  {
    artifact-title: (string-ascii 64),
    current-custodian: principal,
    artifact-size: uint,
    documentation-block: uint,
    artifact-description: (string-ascii 128),
    artifact-classifications: (list 10 (string-ascii 32))
  }
)

;; Archive of authorized authentication experts
(define-map accredited-authenticators
  { authenticator: principal }
  { accreditation-active: bool }
)

;; Documentation of artifact authentication history
(define-map authentication-history
  { artifact-id: uint }
  {
    is-authentic: bool,
    authenticated-by: principal,
    authentication-block: uint,
    authentication-notes: (string-ascii 256)
  }
)

;; Permissions system for artifact viewing rights
(define-map artifact-viewing-permissions
  { artifact-id: uint, viewer: principal }
  { viewing-permitted: bool }
)

;; =========================================================
;; Section 4: Utility Functions
;; =========================================================

;; Retrieves the recorded size of an artifact
(define-private (retrieve-artifact-size (artifact-id uint))
  (default-to u0
    (get artifact-size
      (map-get? artifact-archive { artifact-id: artifact-id })
    )
  )
)

;; Validates that a classification tag meets system specifications
(define-private (is-valid-classification (classification (string-ascii 32)))
  (and
    (> (len classification) u0)
    (< (len classification) u33)
  )
)

;; Validates the integrity of a complete classification list
(define-private (validate-classification-list (classifications (list 10 (string-ascii 32))))
  (and
    (> (len classifications) u0)
    (<= (len classifications) u10)
    (is-eq (len (filter is-valid-classification classifications)) (len classifications))
  )
)

;; Verifies if a given artifact exists in the archive
(define-private (artifact-exists (artifact-id uint))
  (is-some (map-get? artifact-archive { artifact-id: artifact-id }))
)

;; Checks if the caller is the current custodian of an artifact
(define-private (is-artifact-custodian (artifact-id uint) (account principal))
  (match (map-get? artifact-archive { artifact-id: artifact-id })
    artifact (is-eq (get current-custodian artifact) account)
    false
  )
)

;; =========================================================
;; Section 5: Public Documentation Functions
;; =========================================================

;; Document a new artifact in the system
(define-public (document-new-artifact 
                (title (string-ascii 64)) 
                (size uint) 
                (description (string-ascii 128)) 
                (classifications (list 10 (string-ascii 32))))
  (let
    (
      (new-artifact-id (+ (var-get artifact-counter) u1))
    )
    ;; Comprehensive input validation
    (asserts! (> (len title) u0) error-invalid-artifact-title)
    (asserts! (< (len title) u65) error-invalid-artifact-title)
    (asserts! (> size u0) error-insufficient-artifact-size)
    (asserts! (< size u1000000000) error-insufficient-artifact-size)
    (asserts! (> (len description) u0) error-invalid-artifact-title)
    (asserts! (< (len description) u129) error-invalid-artifact-title)
    (asserts! (validate-classification-list classifications) error-invalid-metadata-format)

    ;; Create the new artifact record with complete metadata
    (map-insert artifact-archive
      { artifact-id: new-artifact-id }
      {
        artifact-title: title,
        current-custodian: tx-sender,
        artifact-size: size,
        documentation-block: block-height,
        artifact-description: description,
        artifact-classifications: classifications
      }
    )

    ;; Automatically grant viewing rights to the artifact documenter
    (map-insert artifact-viewing-permissions
      { artifact-id: new-artifact-id, viewer: tx-sender }
      { viewing-permitted: true }
    )

    ;; Update system statistics
    (var-set artifact-counter new-artifact-id)

    ;; Return the new artifact's identifier
    (ok new-artifact-id)
  )
)

;; Transfer custodianship of an artifact to another principal
(define-public (transfer-artifact-custodianship (artifact-id uint) (new-custodian principal))
  (let
    (
      (artifact-data (unwrap! (map-get? artifact-archive { artifact-id: artifact-id }) error-not-found))
    )
    ;; Verify existence and ownership
    (asserts! (artifact-exists artifact-id) error-not-found)
    (asserts! (is-eq (get current-custodian artifact-data) tx-sender) error-unauthorized-action)

    ;; Update the artifact record with the new custodian
    (map-set artifact-archive
      { artifact-id: artifact-id }
      (merge artifact-data { current-custodian: new-custodian })
    )

    (ok true)
  )
)

;; Update an existing artifact's documentation details
(define-public (update-artifact-documentation 
                (artifact-id uint) 
                (revised-title (string-ascii 64)) 
                (revised-size uint) 
                (revised-description (string-ascii 128)) 
                (revised-classifications (list 10 (string-ascii 32))))
  (let
    (
      (artifact-data (unwrap! (map-get? artifact-archive { artifact-id: artifact-id }) error-not-found))
    )
    ;; Validate artifact existence and custodianship
    (asserts! (artifact-exists artifact-id) error-not-found)
    (asserts! (is-eq (get current-custodian artifact-data) tx-sender) error-unauthorized-action)

    ;; Validate all updated information
    (asserts! (> (len revised-title) u0) error-invalid-artifact-title)
    (asserts! (< (len revised-title) u65) error-invalid-artifact-title)
    (asserts! (> revised-size u0) error-insufficient-artifact-size)
    (asserts! (< revised-size u1000000000) error-insufficient-artifact-size)
    (asserts! (> (len revised-description) u0) error-invalid-artifact-title)
    (asserts! (< (len revised-description) u129) error-invalid-artifact-title)
    (asserts! (validate-classification-list revised-classifications) error-invalid-metadata-format)

    ;; Update the artifact record with comprehensive revisions
    (map-set artifact-archive
      { artifact-id: artifact-id }
      (merge artifact-data { 
        artifact-title: revised-title, 
        artifact-size: revised-size, 
        artifact-description: revised-description, 
        artifact-classifications: revised-classifications 
      })
    )

    (ok true)
  )
)

;; Remove an artifact from the documentation system
(define-public (retire-artifact-documentation (artifact-id uint))
  (let
    (
      (artifact-data (unwrap! (map-get? artifact-archive { artifact-id: artifact-id }) error-not-found))
    )
    ;; Verify artifact existence and custodianship rights
    (asserts! (artifact-exists artifact-id) error-not-found)
    (asserts! (is-eq (get current-custodian artifact-data) tx-sender) error-unauthorized-action)

    ;; Permanently remove the artifact from the archive
    (map-delete artifact-archive { artifact-id: artifact-id })

    (ok true)
  )
)

;; =========================================================
;; Section 6: Access Control Management
;; =========================================================

;; Revoke an account's viewing permissions for an artifact
(define-public (revoke-viewing-permission (artifact-id uint) (viewer principal))
  (let
    (
      (artifact-data (unwrap! (map-get? artifact-archive { artifact-id: artifact-id }) error-not-found))
    )
    ;; Validate artifact existence and authority
    (asserts! (artifact-exists artifact-id) error-not-found)
    (asserts! (is-eq (get current-custodian artifact-data) tx-sender) error-unauthorized-action)
    (asserts! (not (is-eq viewer tx-sender)) error-invalid-artifact-title) ;; Custodian cannot revoke own access

    ;; Remove the viewing permission from the system
    (map-delete artifact-viewing-permissions { artifact-id: artifact-id, viewer: viewer })

    (ok true)
  )
)

;; Request access to view an artifact's documentation
(define-public (request-artifact-access (artifact-id uint))
  (let
    (
      (artifact-data (unwrap! (map-get? artifact-archive { artifact-id: artifact-id }) error-not-found))
      (access-rights (map-get? artifact-viewing-permissions { artifact-id: artifact-id, viewer: tx-sender }))
    )
    ;; Confirm artifact exists in the archive
    (asserts! (artifact-exists artifact-id) error-not-found)

    ;; Verify viewing permissions thoroughly
    (asserts! (or 
                (is-eq (get current-custodian artifact-data) tx-sender)
                (is-some access-rights)
                (and (is-some access-rights) (get viewing-permitted (unwrap! access-rights error-access-denied)))
              ) 
              error-access-denied)

    ;; Return the full artifact documentation if authorized
    (ok artifact-data)
  )
)

;; =========================================================
;; Section 7: Authentication Framework
;; =========================================================

;; Authenticate and certify an artifact's provenance
(define-public (authenticate-artifact-provenance (artifact-id uint) (expert-notes (string-ascii 256)))
  (let
    (
      (artifact-data (unwrap! (map-get? artifact-archive { artifact-id: artifact-id }) error-not-found))
      (authenticator-credentials (unwrap! (map-get? accredited-authenticators { authenticator: tx-sender }) error-unauthorized-action))
    )
    ;; Verify artifact documentation exists
    (asserts! (artifact-exists artifact-id) error-not-found)

    ;; Validate authenticator's credentials
    (asserts! (get accreditation-active authenticator-credentials) error-unauthorized-action)

    (ok true)
  )
)

