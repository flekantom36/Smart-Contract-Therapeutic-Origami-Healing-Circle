;; ====================================
;; Smart Contract: Therapeutic Origami Healing Circle
;; ====================================
;; A comprehensive system for coordinating paper folding therapy with instruction sharing,
;; mindfulness practice integration, and fine motor skill development tracking.
;; Includes cultural tradition preservation, accessibility accommodation, and community art creation.

;; ====================================
;; CONTRACT 1: ORIGAMI-HEALING-CORE
;; ====================================

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INVALID-INPUT (err u400))
(define-constant ERR-SESSION-FULL (err u409))
(define-constant ERR-ALREADY-REGISTERED (err u409))
(define-constant ERR-SESSION-NOT-ACTIVE (err u410))

;; Data Variables
(define-data-var next-session-id uint u1)
(define-data-var next-participant-id uint u1)
(define-data-var contract-active bool true)

;; Data Maps

;; Healing Sessions
(define-map healing-sessions
  { session-id: uint }
  {
    facilitator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    origami-pattern: (string-ascii 50),
    cultural-tradition: (string-ascii 50),
    difficulty-level: uint, ;; 1-5 scale
    max-participants: uint,
    current-participants: uint,
    session-start: uint,
    session-duration: uint, ;; in blocks
    mindfulness-focus: (string-ascii 100),
    accessibility-features: (list 5 (string-ascii 50)),
    is-active: bool,
    community-art-theme: (optional (string-ascii 100))
  }
)

;; Participant Profiles
(define-map participant-profiles
  { participant: principal }
  {
    participant-id: uint,
    name: (string-ascii 50),
    skill-level: uint, ;; 1-5 scale
    accessibility-needs: (list 5 (string-ascii 50)),
    cultural-interests: (list 3 (string-ascii 50)),
    sessions-completed: uint,
    total-mindfulness-minutes: uint,
    fine-motor-progress: uint, ;; 1-100 scale
    preferred-traditions: (list 3 (string-ascii 50)),
    registration-block: uint
  }
)

;; Session Participation
(define-map session-participants
  { session-id: uint, participant: principal }
  {
    joined-at: uint,
    completion-status: (string-ascii 20), ;; "in-progress", "completed", "dropped"
    skill-improvement: uint, ;; 1-10 scale
    mindfulness-rating: uint, ;; 1-10 scale
    cultural-learning: uint, ;; 1-10 scale
    fine-motor-assessment: uint, ;; 1-100 scale
    feedback: (string-ascii 300),
    origami-completed: bool,
    meditation-participation: bool
  }
)

;; Origami Instructions Repository
(define-map origami-instructions
  { pattern-id: (string-ascii 50) }
  {
    creator: principal,
    pattern-name: (string-ascii 100),
    cultural-origin: (string-ascii 50),
    therapeutic-benefits: (list 5 (string-ascii 100)),
    difficulty-level: uint,
    estimated-time: uint, ;; in minutes
    accessibility-adaptations: (list 5 (string-ascii 100)),
    step-count: uint,
    mindfulness-cues: (list 10 (string-ascii 100)),
    fine-motor-skills: (list 5 (string-ascii 50)),
    cultural-significance: (string-ascii 300),
    creation-block: uint,
    usage-count: uint,
    average-rating: uint ;; 1-100 scale
  }
)

;; Community Art Projects
(define-map community-art-projects
  { project-id: uint }
  {
    coordinator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    target-participants: uint,
    current-contributions: uint,
    start-block: uint,
    end-block: uint,
    cultural-theme: (string-ascii 50),
    therapeutic-goal: (string-ascii 100),
    accessibility-focus: (list 3 (string-ascii 50)),
    is-complete: bool,
    final-artwork-hash: (optional (buff 32))
  }
)

;; Cultural Tradition Registry
(define-map cultural-traditions
  { tradition-id: (string-ascii 50) }
  {
    preservationist: principal,
    tradition-name: (string-ascii 100),
    origin-country: (string-ascii 50),
    historical-context: (string-ascii 500),
    therapeutic-applications: (list 5 (string-ascii 100)),
    key-patterns: (list 10 (string-ascii 50)),
    meditation-practices: (list 5 (string-ascii 100)),
    accessibility-considerations: (string-ascii 300),
    preservation-urgency: uint, ;; 1-5 scale
    community-practitioners: uint,
    documentation-completeness: uint ;; 1-100 scale
  }
)

;; Read-only functions

(define-read-only (get-session-details (session-id uint))
  (map-get? healing-sessions { session-id: session-id })
)

(define-read-only (get-participant-profile (participant principal))
  (map-get? participant-profiles { participant: participant })
)

(define-read-only (get-session-participation (session-id uint) (participant principal))
  (map-get? session-participants { session-id: session-id, participant: participant })
)

(define-read-only (get-origami-instructions (pattern-id (string-ascii 50)))
  (map-get? origami-instructions { pattern-id: pattern-id })
)

(define-read-only (get-community-art-project (project-id uint))
  (map-get? community-art-projects { project-id: project-id })
)

(define-read-only (get-cultural-tradition (tradition-id (string-ascii 50)))
  (map-get? cultural-traditions { tradition-id: tradition-id })
)

(define-read-only (get-current-session-id)
  (var-get next-session-id)
)

(define-read-only (is-session-active (session-id uint))
  (match (map-get? healing-sessions { session-id: session-id })
    session-data
      (and
        (get is-active session-data)
        (< (+ (get session-start session-data) (get session-duration session-data)) stacks-block-height)
      )
    false
  )
)

(define-read-only (calculate-participant-wellness-score (participant principal))
  (match (map-get? participant-profiles { participant: participant })
    profile-data
      (let (
        (sessions (get sessions-completed profile-data))
        (mindfulness (get total-mindfulness-minutes profile-data))
        (motor-skills (get fine-motor-progress profile-data))
      )
        (ok (+ (* sessions u10) (/ mindfulness u60) motor-skills))
      )
    (err u404)
  )
)

;; Public functions

;; Register as a participant
(define-public (register-participant
  (name (string-ascii 50))
  (skill-level uint)
  (accessibility-needs (list 5 (string-ascii 50)))
  (cultural-interests (list 3 (string-ascii 50)))
  (preferred-traditions (list 3 (string-ascii 50)))
)
  (let (
    (participant-id (var-get next-participant-id))
  )
    (asserts! (is-none (map-get? participant-profiles { participant: tx-sender })) ERR-ALREADY-REGISTERED)
    (asserts! (and (>= skill-level u1) (<= skill-level u5)) ERR-INVALID-INPUT)
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)

    (map-set participant-profiles
      { participant: tx-sender }
      {
        participant-id: participant-id,
        name: name,
        skill-level: skill-level,
        accessibility-needs: accessibility-needs,
        cultural-interests: cultural-interests,
        sessions-completed: u0,
        total-mindfulness-minutes: u0,
        fine-motor-progress: u50, ;; Start at middle baseline
        preferred-traditions: preferred-traditions,
        registration-block: stacks-block-height
      }
    )

    (var-set next-participant-id (+ participant-id u1))
    (ok participant-id)
  )
)

;; Create a healing session
(define-public (create-healing-session
  (title (string-ascii 100))
  (description (string-ascii 500))
  (origami-pattern (string-ascii 50))
  (cultural-tradition (string-ascii 50))
  (difficulty-level uint)
  (max-participants uint)
  (session-duration uint)
  (mindfulness-focus (string-ascii 100))
  (accessibility-features (list 5 (string-ascii 50)))
  (community-art-theme (optional (string-ascii 100)))
)
  (let (
    (session-id (var-get next-session-id))
  )
    (asserts! (is-some (map-get? participant-profiles { participant: tx-sender })) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= difficulty-level u1) (<= difficulty-level u5)) ERR-INVALID-INPUT)
    (asserts! (and (> max-participants u0) (<= max-participants u50)) ERR-INVALID-INPUT)
    (asserts! (> session-duration u0) ERR-INVALID-INPUT)
    (asserts! (> (len title) u0) ERR-INVALID-INPUT)

    (map-set healing-sessions
      { session-id: session-id }
      {
        facilitator: tx-sender,
        title: title,
        description: description,
        origami-pattern: origami-pattern,
        cultural-tradition: cultural-tradition,
        difficulty-level: difficulty-level,
        max-participants: max-participants,
        current-participants: u0,
        session-start: stacks-block-height,
        session-duration: session-duration,
        mindfulness-focus: mindfulness-focus,
        accessibility-features: accessibility-features,
        is-active: true,
        community-art-theme: community-art-theme
      }
    )

    (var-set next-session-id (+ session-id u1))
    (ok session-id)
  )
)

;; Join a healing session
(define-public (join-healing-session (session-id uint))
  (let (
    (session-data (unwrap! (map-get? healing-sessions { session-id: session-id }) ERR-NOT-FOUND))
    (participant-data (unwrap! (map-get? participant-profiles { participant: tx-sender }) ERR-NOT-AUTHORIZED))
  )
    (asserts! (get is-active session-data) ERR-SESSION-NOT-ACTIVE)
    (asserts! (< (get current-participants session-data) (get max-participants session-data)) ERR-SESSION-FULL)
    (asserts! (is-none (map-get? session-participants { session-id: session-id, participant: tx-sender })) ERR-ALREADY-REGISTERED)

    ;; Add participant to session
    (map-set session-participants
      { session-id: session-id, participant: tx-sender }
      {
        joined-at: stacks-block-height,
        completion-status: "in-progress",
        skill-improvement: u0,
        mindfulness-rating: u0,
        cultural-learning: u0,
        fine-motor-assessment: (get fine-motor-progress participant-data),
        feedback: "",
        origami-completed: false,
        meditation-participation: false
      }
    )

    ;; Update session participant count
    (map-set healing-sessions
      { session-id: session-id }
      (merge session-data { current-participants: (+ (get current-participants session-data) u1) })
    )

    (ok true)
  )
)

;; Complete session participation
(define-public (complete-session-participation
  (session-id uint)
  (skill-improvement uint)
  (mindfulness-rating uint)
  (cultural-learning uint)
  (fine-motor-assessment uint)
  (feedback (string-ascii 300))
  (meditation-participation bool)
)
  (let (
    (participation-data (unwrap! (map-get? session-participants { session-id: session-id, participant: tx-sender }) ERR-NOT-FOUND))
    (participant-data (unwrap! (map-get? participant-profiles { participant: tx-sender }) ERR-NOT-FOUND))
    (session-data (unwrap! (map-get? healing-sessions { session-id: session-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq (get completion-status participation-data) "in-progress") ERR-INVALID-INPUT)
    (asserts! (and (>= skill-improvement u1) (<= skill-improvement u10)) ERR-INVALID-INPUT)
    (asserts! (and (>= mindfulness-rating u1) (<= mindfulness-rating u10)) ERR-INVALID-INPUT)
    (asserts! (and (>= cultural-learning u1) (<= cultural-learning u10)) ERR-INVALID-INPUT)
    (asserts! (and (>= fine-motor-assessment u1) (<= fine-motor-assessment u100)) ERR-INVALID-INPUT)

    ;; Update session participation
    (map-set session-participants
      { session-id: session-id, participant: tx-sender }
      (merge participation-data {
        completion-status: "completed",
        skill-improvement: skill-improvement,
        mindfulness-rating: mindfulness-rating,
        cultural-learning: cultural-learning,
        fine-motor-assessment: fine-motor-assessment,
        feedback: feedback,
        origami-completed: true,
        meditation-participation: meditation-participation
      })
    )

    ;; Update participant profile
    (map-set participant-profiles
      { participant: tx-sender }
      (merge participant-data {
        sessions-completed: (+ (get sessions-completed participant-data) u1),
        total-mindfulness-minutes: (+ (get total-mindfulness-minutes participant-data) (if meditation-participation u30 u0)),
        fine-motor-progress: fine-motor-assessment
      })
    )

    (ok true)
  )
)

;; Add origami instructions
(define-public (add-origami-instructions
  (pattern-id (string-ascii 50))
  (pattern-name (string-ascii 100))
  (cultural-origin (string-ascii 50))
  (therapeutic-benefits (list 5 (string-ascii 100)))
  (difficulty-level uint)
  (estimated-time uint)
  (accessibility-adaptations (list 5 (string-ascii 100)))
  (step-count uint)
  (mindfulness-cues (list 10 (string-ascii 100)))
  (fine-motor-skills (list 5 (string-ascii 50)))
  (cultural-significance (string-ascii 300))
)
  (begin
    (asserts! (is-some (map-get? participant-profiles { participant: tx-sender })) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? origami-instructions { pattern-id: pattern-id })) ERR-ALREADY-REGISTERED)
    (asserts! (and (>= difficulty-level u1) (<= difficulty-level u5)) ERR-INVALID-INPUT)
    (asserts! (> (len pattern-name) u0) ERR-INVALID-INPUT)
    (asserts! (> step-count u0) ERR-INVALID-INPUT)

    (map-set origami-instructions
      { pattern-id: pattern-id }
      {
        creator: tx-sender,
        pattern-name: pattern-name,
        cultural-origin: cultural-origin,
        therapeutic-benefits: therapeutic-benefits,
        difficulty-level: difficulty-level,
        estimated-time: estimated-time,
        accessibility-adaptations: accessibility-adaptations,
        step-count: step-count,
        mindfulness-cues: mindfulness-cues,
        fine-motor-skills: fine-motor-skills,
        cultural-significance: cultural-significance,
        creation-block: stacks-block-height,
        usage-count: u0,
        average-rating: u0
      }
    )

    (ok true)
  )
)

;; Preserve cultural tradition
(define-public (preserve-cultural-tradition
  (tradition-id (string-ascii 50))
  (tradition-name (string-ascii 100))
  (origin-country (string-ascii 50))
  (historical-context (string-ascii 500))
  (therapeutic-applications (list 5 (string-ascii 100)))
  (key-patterns (list 10 (string-ascii 50)))
  (meditation-practices (list 5 (string-ascii 100)))
  (accessibility-considerations (string-ascii 300))
  (preservation-urgency uint)
  (documentation-completeness uint)
)
  (begin
    (asserts! (is-some (map-get? participant-profiles { participant: tx-sender })) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? cultural-traditions { tradition-id: tradition-id })) ERR-ALREADY-REGISTERED)
    (asserts! (and (>= preservation-urgency u1) (<= preservation-urgency u5)) ERR-INVALID-INPUT)
    (asserts! (and (>= documentation-completeness u1) (<= documentation-completeness u100)) ERR-INVALID-INPUT)
    (asserts! (> (len tradition-name) u0) ERR-INVALID-INPUT)

    (map-set cultural-traditions
      { tradition-id: tradition-id }
      {
        preservationist: tx-sender,
        tradition-name: tradition-name,
        origin-country: origin-country,
        historical-context: historical-context,
        therapeutic-applications: therapeutic-applications,
        key-patterns: key-patterns,
        meditation-practices: meditation-practices,
        accessibility-considerations: accessibility-considerations,
        preservation-urgency: preservation-urgency,
        community-practitioners: u1,
        documentation-completeness: documentation-completeness
      }
    )

    (ok true)
  )
)

;; Emergency pause (owner only)
(define-public (emergency-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set contract-active false)
    (ok true)
  )
)

;; Resume operations (owner only)
(define-public (resume-operations)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set contract-active true)
    (ok true)
  )
)

;; ====================================
;; CONTRACT 2: ORIGAMI-COMMUNITY-REWARDS
;; ====================================

;; Constants for rewards
(define-constant COMPLETION-REWARD u100)
(define-constant TEACHING-REWARD u200)
(define-constant CULTURAL-PRESERVATION-REWARD u500)
(define-constant ACCESSIBILITY-CONTRIBUTION-REWARD u300)

;; Token-like rewards system
(define-fungible-token healing-points)
(define-data-var total-healing-points-minted uint u0)

;; Achievement tracking
(define-map participant-achievements
  { participant: principal }
  {
    total-points: uint,
    sessions-facilitated: uint,
    patterns-taught: uint,
    accessibility-contributions: uint,
    cultural-preservations: uint,
    community-art-participations: uint,
    mindfulness-master: bool,
    fine-motor-mentor: bool,
    cultural-ambassador: bool,
    accessibility-advocate: bool
  }
)

;; Milestone rewards
(define-map milestone-rewards
  { milestone: (string-ascii 50) }
  {
    points-required: uint,
    reward-amount: uint,
    title: (string-ascii 100),
    badge-description: (string-ascii 200)
  }
)

;; Initialize milestone rewards
(map-set milestone-rewards { milestone: "beginner-healer" }
  { points-required: u100, reward-amount: u50, title: "Beginner Healer", badge-description: "Completed first origami healing session" })
(map-set milestone-rewards { milestone: "mindfulness-practitioner" }
  { points-required: u500, reward-amount: u100, title: "Mindfulness Practitioner", badge-description: "Achieved 300+ minutes of mindful folding" })
(map-set milestone-rewards { milestone: "cultural-student" }
  { points-required: u300, reward-amount: u75, title: "Cultural Student", badge-description: "Learned patterns from 3+ cultural traditions" })
(map-set milestone-rewards { milestone: "community-builder" }
  { points-required: u1000, reward-amount: u200, title: "Community Builder", badge-description: "Facilitated 5+ healing sessions" })
(map-set milestone-rewards { milestone: "accessibility-champion" }
  { points-required: u800, reward-amount: u150, title: "Accessibility Champion", badge-description: "Created accessibility adaptations for 3+ patterns" })

;; Read-only functions for rewards
(define-read-only (get-participant-achievements (participant principal))
  (map-get? participant-achievements { participant: participant })
)

(define-read-only (get-healing-points-balance (participant principal))
  (ft-get-balance healing-points participant)
)

(define-read-only (get-milestone-reward (milestone (string-ascii 50)))
  (map-get? milestone-rewards { milestone: milestone })
)

;; Award points for session completion
(define-public (award-completion-points (participant principal) (session-id uint))
  (let (
    (current-achievements (default-to
      { total-points: u0, sessions-facilitated: u0, patterns-taught: u0,
        accessibility-contributions: u0, cultural-preservations: u0,
        community-art-participations: u0, mindfulness-master: false,
        fine-motor-mentor: false, cultural-ambassador: false, accessibility-advocate: false }
      (map-get? participant-achievements { participant: participant })
    ))
  )
    ;; Only the healing session contract can award these points
    (try! (ft-mint? healing-points COMPLETION-REWARD participant))

    (map-set participant-achievements
      { participant: participant }
      (merge current-achievements {
        total-points: (+ (get total-points current-achievements) COMPLETION-REWARD)
      })
    )

    (var-set total-healing-points-minted (+ (var-get total-healing-points-minted) COMPLETION-REWARD))
    (ok true)
  )
)

;; Award points for teaching/facilitating
(define-public (award-teaching-points (facilitator principal))
  (let (
    (current-achievements (default-to
      { total-points: u0, sessions-facilitated: u0, patterns-taught: u0,
        accessibility-contributions: u0, cultural-preservations: u0,
        community-art-participations: u0, mindfulness-master: false,
        fine-motor-mentor: false, cultural-ambassador: false, accessibility-advocate: false }
      (map-get? participant-achievements { participant: facilitator })
    ))
  )
    (try! (ft-mint? healing-points TEACHING-REWARD facilitator))

    (map-set participant-achievements
      { participant: facilitator }
      (merge current-achievements {
        total-points: (+ (get total-points current-achievements) TEACHING-REWARD),
        sessions-facilitated: (+ (get sessions-facilitated current-achievements) u1)
      })
    )

    (var-set total-healing-points-minted (+ (var-get total-healing-points-minted) TEACHING-REWARD))
    (ok true)
  )
)

;; Award points for cultural preservation
(define-public (award-preservation-points (preservationist principal))
  (let (
    (current-achievements (default-to
      { total-points: u0, sessions-facilitated: u0, patterns-taught: u0,
        accessibility-contributions: u0, cultural-preservations: u0,
        community-art-participations: u0, mindfulness-master: false,
        fine-motor-mentor: false, cultural-ambassador: false, accessibility-advocate: false }
      (map-get? participant-achievements { participant: preservationist })
    ))
  )
    (try! (ft-mint? healing-points CULTURAL-PRESERVATION-REWARD preservationist))

    (map-set participant-achievements
      { participant: preservationist }
      (merge current-achievements {
        total-points: (+ (get total-points current-achievements) CULTURAL-PRESERVATION-REWARD),
        cultural-preservations: (+ (get cultural-preservations current-achievements) u1)
      })
    )

    (var-set total-healing-points-minted (+ (var-get total-healing-points-minted) CULTURAL-PRESERVATION-REWARD))
    (ok true)
  )
)

;; Check and award milestone achievements
(define-public (check-milestone-achievements (participant principal))
  (let (
    (achievements (unwrap! (map-get? participant-achievements { participant: participant }) ERR-NOT-FOUND))
    (total-points (get total-points achievements))
  )
    ;; Check various milestones and award badges
    (if (and (>= total-points u100) (< total-points u200))
      (try! (ft-mint? healing-points u50 participant))
      true
    )

    (if (and (>= total-points u500) (< total-points u600))
      (try! (ft-mint? healing-points u100 participant))
      true
    )

    (if (and (>= total-points u1000) (< total-points u1100))
      (try! (ft-mint? healing-points u200 participant))
      true
    )

    (ok true)
  )
)
