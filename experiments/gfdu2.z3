;; Proof that running GetFeed at RR is sufficient. 
;; -----------------------------------------------------------------
;;                    Prelude
;; -----------------------------------------------------------------
(declare-sort TString)
(declare-sort TId)
(declare-fun stringLength (TString) TId)
;; Models as uninterpreted types
;; User model
(declare-sort User)
(declare-fun userId (User) TId)
(declare-fun userName (User) TString)
(declare-fun userAdmin (User) Bool)
;; Relationship model
(declare-sort Rel)
(declare-fun relId (Rel) TId)
(declare-fun relFollowerId (Rel) TId)
(declare-fun relFollowedId (Rel) TId)
;; Post model
(declare-sort Post)
(declare-fun postId (Post) TId)
(declare-fun postUserId (Post) TId)
;; Database relations
(declare-fun Rrel (Rel) Bool)
(declare-fun Ruser (User) Bool)
(declare-fun Rpost (Post) Bool)
;; Uniqueness of Ids.
(assert (forall ((u1 User)(u2 User)) 
                    (=> (not (= u1 u2))
                        (not (= (userId u1) (userId u2))))))
(assert (forall ((r1 Rel)(r2 Rel)) 
                    (=> (not (= r1 r2))
                        (not (= (relId r1) (relId r2))))))
(assert (forall ((p1 Post)(p2 Post)) 
                    (=> (not (= p1 p2))
                        (not (= (postId p1) (postId p2))))))

;; -----------------------------------------------------------------
;;                    Effects and Eff Relations
;; -----------------------------------------------------------------
(declare-datatypes () ((ObjType USER POST REL)))
(declare-datatypes () ((EffKind RD WR)))
(declare-datatypes () ((Oper GF DU))) 
(declare-sort Effect)
(declare-fun effObjId (Effect) TId)
(declare-fun effObjType (Effect) ObjType)
(declare-fun effKind (Effect) EffKind)
(declare-fun effOper (Effect) Oper)
;; sameobj relation
(declare-fun sameobj (Effect Effect) Bool)
;; sameobj relates effects whose ObjType and ObjId are same.
(assert (forall ((eff1 Effect)(eff2 Effect)) 
                    (= (sameobj eff1 eff2) 
                       (and (= (effObjId eff1) (effObjId eff2))
                            (= (effObjType eff1) (effObjType eff2))))))
;; visibility relation
(declare-fun vis (Effect Effect) Bool)
;; visiblity => sameobj
(assert (forall ((eff1 Effect)(eff2 Effect)) 
                    (=> (vis eff1 eff2) (sameobj eff1 eff2))))
;; visibility is acyclic
(assert (forall ((eff Effect)) (not (vis eff eff))))

;; -----------------------------------------------------------------
;;                    getFeed(usr1)
;; -----------------------------------------------------------------
;;  π_{[]}(
;;    σ_{\x.Or (InRel (DotExp (Var_T "x") (Field_T "user_id")) 
;;                    π_{[Field_T "followed_id"]}(
;;                        σ_{\x.Eq (DotExp (Var_T "x") (Field_T "follower_id"))
;;                                 (DotExp (Var_T "x") (Field_T "v0"))}
;;                          (R_{relationships}))) 
;;            (Eq (DotExp (Var_T "x") (Field_T "user_id")) 
;;                (DotExp (Var_T "x") (Field_T "v0")))}
;;      (R_{microposts})

;; Let us begin with our getFeed program
(declare-const usr1 User)
(declare-const v0 TId)
(assert (= v0 (userId usr1)))

;; The type of the result of the query.
(declare-sort TPostList)
(declare-const v8 TPostList)
(declare-fun RmemPost (TPostList Post) Bool)
;; Let us consider the subquery:
;;    σ_{\x.Eq (DotExp (Var_T "x") (Field_T "follower_id"))
;;             (DotExp (Var_T "x") (Field_T "v0"))} (R_{relationships})
(define-fun p1 ((x Rel)) Bool
  (= (relFollowerId x) v0)) ; correction: it's not x.v0, rather just v0.
(declare-fun S0 (Rel) Bool)
(assert (forall ((bv0 Rel)) (= (S0 bv0) (and (Rrel bv0) (p1 bv0)))))
;; Next, consider the projection:
;;    π_{[Field_T "followed_id"]}( ... )

;; declare a temporary relation
;; (declare-datatypes () ((TR0 (mk-tr0 (follower_id TId)))))
(declare-sort TR0)
(declare-fun tr0FollowerId (TR0) TId)
(declare-fun S1 (TR0) Bool)
;; Forall bv0:Rel and bv1:TR0, if bv0 \in S0, and bv1 can be
;; constructed from bv0, then bv1 must belong to S1. (This is how the
;; projection actually works)
(assert (forall ((bv0 Rel)(bv1 TR0)) (=> (and (S0 bv0) 
                                              (= (tr0FollowerId bv1)
                                                 (relFollowerId bv0)))
                                          (S1 bv1) )))
;; However, if bv1 \in S1 and bv0 is any Rel from which bv1 can be
;; constructed, then bv0 need not necessarily belong to S0. For each
;; bv1 \in S1, there exists at least one bv0 in S0 which could've
;; constructed bv1.
(assert (forall ((bv1 TR0)) 
          (=> (S1 bv1)
              (exists ((bv0 Rel)) (and (S0 bv0) 
                                    (= (tr0FollowerId bv1)
                                       (relFollowerId bv0))) ))))
;; Now, consider the top-level selection predicate ("or")
(define-fun p2 ((x Post)) Bool
  (or (exists ((tr0 TR0)) (and (= (tr0FollowerId tr0) 
                                  (postUserId x)) 
                               (S1 tr0)))
      (= (postUserId x) v0)))
;; And the top-level relation:
(declare-fun S2 (Post) Bool)
(assert (forall ((bv0 Post)) (= (S2 bv0) (and (Rpost bv0) (p2 bv0)))))
;; v8 Rmem is S2
(assert (forall ((bv0 Post)) (= (RmemPost v8 bv0) (S2 bv0))))

;; v13 := v8.map (\x. x.user_id)
(declare-sort TUserIdList)
(declare-const v13 TUserIdList)
(declare-fun RmemUserId (TUserIdList TId) Bool)
(assert (forall ((bv0 Post)) (=> (RmemPost v8 bv0)
                                 (RmemUserId v13 (postUserId bv0)) )))
(assert (forall ((bv1 TId)) 
          (=> (RmemUserId v13 bv1)
              (exists ((bv0 Post)) (and (RmemPost v8 bv0) 
                                        (= (postUserId bv0) bv1)) ))))
;; SELECT "users".* FROM "users"  WHERE "users"."id" IN ('v13')
;; π_{[]}(σ_{\x.In (DotExp (Var_T "users") (Field_T "id")) [DotExp
;; (Var_T "x") (Field_T "v13")]}(R_{users})
(define-fun p3 ((x User)) Bool
  (RmemUserId v13 (userId x))
)
(declare-fun S3 (User) Bool)
(assert (forall ((bv0 User)) (= (S3 bv0) (and (Ruser bv0) (p3 bv0)))))
(declare-sort TUserList)
(declare-fun RmemUser (TUserList User) Bool)
(declare-const v15 TUserList)
;; v15 Rmem is S3
(assert (forall ((bv0 User)) (= (RmemUser v15 bv0) (S3 bv0))))
;; All effects with effOper = GF are RDs on S2 and S3

;; EffSet2 is EffSet correpsonding to S2.
(declare-fun EffSet2 (Effect) Bool)
;; All effects in EffSet2 are RDs on POSTs.
(assert (forall ((e Effect)) 
            (=> (EffSet2 e)
                (and (= (effObjType e) POST)
                     (= (effKind e) RD)
                     ;(= (effOper e) GF)
                     ))))
;; Soundness of EffSet2
(assert (forall ((e Effect)) 
            (=> (EffSet2 e)
                (exists ((p Post))
                            (and (S2 p) (= (postId p) (effObjId e)))))))
;; We may not need the below assertion. 
;; It asserts the completeness of EffSet2 in capturing effects over S2.
;;  (assert (forall ((p Post)(e Effect)) 
;;              (=> (and (S2 p) 
;;                       (= (effObjType e) POST)
;;                       (= (effKind e) RD)
;;                       (= (effOper e) GF)
;;                       (= (postId p)
;;                          (effObjId e)))
;;                   (EffSet2 e))))

;; EffSet3 is the EffSet corresponding to S3
(declare-fun EffSet3 (Effect) Bool)
;; All effects in EffSet3 are RDs on USERs.
(assert (forall ((e Effect)) 
            (=> (EffSet3 e)
                (and (= (effObjType e) USER)
                     (= (effKind e) RD)
                     ;(= (effOper e) GF)
                     ))))
;; Soundness of EffSet3
(assert (forall ((e Effect)) 
            (=> (EffSet3 e)
                (exists ((u User))
                            (and (S3 u) (= (userId u) (effObjId e)))))))
;; Completeness of EffSet3 in capturing effects over S3 is NOT
;; asserted.

;; GF effects = EffSet2 U EffSet3
(assert (forall ((e Effect)) (= (= (effOper e) GF)
                                 (or (EffSet2 e) (EffSet3 e)))))


;; -----------------------------------------------------------------
;;                    deleteUser(usr2)
;; -----------------------------------------------------------------
(declare-const usr2 User)
(declare-fun S4 (Post) Bool)
(define-fun p5 ((x Post)) Bool
  (= (postUserId x) (userId usr2))
)
;; S4 = SELECT * FROM microposts WHERE user_id = ‘usr2.id‘"
(assert (forall ((bv0 Post)) (= (S4 bv0) (and (Rpost bv0) (p5 bv0)))))
(declare-fun S5 (Rel) Bool)
(define-fun p6 ((x Rel)) Bool
  (or (= (relFollowerId x) (userId usr2))
      (= (relFollowedId x) (userId usr2)))
)
;; S5 = SQL "SELECT * FROM relationships WHERE 
;;                follower_id = ‘usr.id‘ OR followed_id = ‘usr2.id‘";
(assert (forall ((bv0 Rel)) (= (S5 bv0) (and (Rrel bv0) (p6 bv0)))))
(declare-fun S6 (User) Bool)
(define-fun p7 ((x User)) Bool
  (= (userId x) (userId usr2))
)
;; S6 = "SELECT * FROM users WHERE id = ‘usr2.id‘""
(assert (forall ((bv0 User)) (= (S6 bv0) (and (Ruser bv0) (p7 bv0)))))
;; --- Effects of DU ----
;; All effects with effOper = DU are WRs on S4-6

;; EffSet4 is EffSet correpsonding to S4.
(declare-fun EffSet4 (Effect) Bool)
;; All effects in EffSet4 are WRs on POSTs.
(assert (forall ((e Effect)) 
            (=> (EffSet4 e)
                (and (= (effObjType e) POST)
                     (= (effKind e) WR)
                     ;(= (effOper e) DU)
                     ))))
;; Soundness of EffSet4
(assert (forall ((e Effect)) 
            (=> (EffSet4 e)
                (exists ((p Post))
                            (and (S4 p) (= (postId p) (effObjId e)))))))
;; Completeness of EffSet4 w.r.t effects of S4 is not asserted.

;; EffSet5 is EffSet corresponding to S5.
(declare-fun EffSet5 (Effect) Bool)
;; All effects in EffSet5 are WRs on RELs.
(assert (forall ((e Effect)) 
            (=> (EffSet5 e)
                (and (= (effObjType e) REL)
                     (= (effKind e) WR)
                     ;(= (effOper e) DU)
                     ))))
;; Soundness of EffSet5
(assert (forall ((e Effect)) 
            (=> (EffSet5 e)
                (exists ((r Rel))
                            (and (S5 r) (= (relId r) (effObjId e)))))))
;; Completeness of EffSet5 w.r.t effects of S5 is not asserted.

;; EffSet6 is EffSet corresponding to S6.
(declare-fun EffSet6 (Effect) Bool)
;; All effects in EffSet6 are WRs on USERs.
(assert (forall ((e Effect)) 
            (=> (EffSet6 e)
                (and (= (effObjType e) USER)
                     (= (effKind e) WR)
                     ;(= (effOper e) DU)
                     ))))
;; Soundness of EffSet6
(assert (forall ((e Effect)) 
            (=> (EffSet6 e)
                (exists ((u User))
                            (and (S6 u) (= (userId u) (effObjId e)))))))
;; Completeness of EffSet6 w.r.t effects of S6 is not asserted.

;; DU effects = EffSet4 U EffSet5 U EffSet6
(assert (forall ((e Effect)) (= (= (effOper e) DU)
                                 (or (EffSet4 e) (EffSet5 e) (EffSet6 e)))))

;; -----------------------------------------------------------------
;;                      No Conflict
;; -----------------------------------------------------------------
;; Define helper functions
(define-fun txn ((T Oper)(e Effect)) Bool
  (= (effOper e) T)
)
(define-fun isRead ((e Effect)) Bool
  (= (effKind e) RD)
)
(define-fun isWrite ((e Effect)) Bool
  (= (effKind e) WR)
)
;; Define txnvis between txns
(define-fun txnvis ((T1 Oper)(T2 Oper)) Bool
  (forall ((e1 Effect)(e2 Effect))
      (=> (and (txn T1 e1) (txn T2 e2) (sameobj e1 e2))
          (and (=> (and (isWrite e1) (isRead e2)) 
                   (vis e1 e2)) 
               (=> (and (isWrite e2) (isRead e1)) 
                   (not (vis e2 e1))))))
)
;; Define no-conflict condition.
(define-fun no-conflict ((T1 Oper)(T2 Oper)) Bool
  (or (txnvis T1 T2) (txnvis T2 T1))
)
;; Define RR
(define-fun RR((T1 Oper)) Bool
  (forall ((T2 Oper)(e1 Effect)(e2 Effect)) 
    (=> (and (not (= T1 T2)) (txn T1 e1) (txn T2 e2)
                  (isRead e1) (isWrite e2) (vis e2 e1))
        (txnvis T2 T1)))
)

;; Verify no-conflict(GF,DU)
(push)
(assert (not (no-conflict GF DU)))
(check-sat) ;; SAT
(pop)

;; Now, let GF be RR
(push)
(assert (RR GF))
(assert (not (no-conflict GF DU)))
(check-sat) ;; UNSAT
(pop)
;(get-model)
