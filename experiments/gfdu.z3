;; GetFeed-DeleteUser failed experiment.
;; Using Z3's datatype functionality to define records results in
;; check-sat getting timedout.

;; Prelude types and functions, to begin with.
(declare-sort TString)
(declare-sort TInt)
(declare-fun stringLength (TString) TInt)
;; Models as datatypes
(declare-datatypes () ((User (mk-user (id TInt) (name TString) (admin Bool)))))
(declare-datatypes () ((Rel (mk-rel (id TInt) (follower_id TInt) (followed_id TInt)))))
(declare-datatypes () ((Post (mk-post (id TInt) (content TString) (user_id TInt)))))
;; Database relations
(declare-fun Rrel (Rel) Bool)
(declare-fun Ruser (User) Bool)
(declare-fun Rpost (Post) Bool)

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
(declare-const usr User)
(declare-const v0 TInt)
(assert (= v0 (id usr)))

;; The type of the result of the query.
(declare-sort TPostList)
(declare-const v8 TPostList)
(declare-fun RmemPost (TPostList Post) Bool)
;; Let us consider the subquery:
;;    σ_{\x.Eq (DotExp (Var_T "x") (Field_T "follower_id"))
;;             (DotExp (Var_T "x") (Field_T "v0"))} (R_{relationships})
(define-fun p1 ((x Rel)) Bool
  (= (follower_id x) v0)) ; correction: it's not x.v0, rather just v0.
(declare-fun S0 (Rel) Bool)
(assert (forall ((bv0 Rel)) (= (S0 bv0) (and (Rrel bv0) (p1 bv0)))))
;; Next, consider the projection:
;;    π_{[Field_T "followed_id"]}( ... )
(declare-datatypes () ((TR0 (mk-tr0 (follower_id TInt)))))
(declare-fun S1 (TR0) Bool)
(assert (forall ((bv0 Rel)) (= (S0 bv0) (S1 (mk-tr0 (follower_id bv0))))))
(assert (not (forall ((bv0 TR0)) 
                (=> (S1 bv0) 
                   (exists ((bv1 TInt)(bv2 TInt)) 
                      (S0 (mk-rel bv1 (follower_id bv0) bv2)))))))
(check-sat)
; (get-model)




;; Sanity checks from gfdu2.z3
(declare-const a Rel)
(declare-const b TR0)
(assert (= (tr0FollowerId b) (relFollowerId a)))
; (assert (and (S0 a) (not (S1 b))))
(assert (and (S1 b) (not (S0 a))))

