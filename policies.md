# Example Policy Refinements

## HandlerT refinements

You can find the refinements for HandlerT [here](https://github.com/jbrown215/binah/blob/master/policies/Policies.hs#L18-L34)

## Add/Delete/Edit policies

Only an admin can add instructors.


All of the add/delete policies will be similar to this one. 
The only time you need to *propagate* policies is when you are *viewing*
sensitive information. Here, we just need to check permissions of the input
user to make sure they are allowed to perform the action.


I'm not sure how you represent the fact that a user is an admin, but assuming there was some field on the record `isAdmin`, this would suffice.
```
{-@ addInstructors :: {u:User | isAdmin u} -> Class -> [Instructors] -> Handler () @-}
```

## View policies

Only a teacher can view the students in a class.

Here, we need to make use of the (bounded? abstract?) refinements to propagate policies on HandlerT.

```
{-@ viewStudents :: c:Class -> HandlerT <{\u -> teachesClass u c}> App IO [Student] @-}
```
where `teachesClass` is some measure that you guarantee whenever a user u teaches a class c.


## Verifying policies

Any time you want to verify a policy is being satisfied, you need to create some
safe output sink. We don't have a convenient way of doing that just yet (see below).
In the meantime, you can use a function like this to verify that a user can access the
data being guarded by some HandlerT:

```
{-@ verifyPolicy :: HandlerT <p> App IO a -> User <p> -> () @-}
verifyPolicy :: Handler a -> User -> ()
verifyPolicy _ _ = ()
```

Unfortunately, I have not yet dug into the internals of Yesod/Hamlet/Julius to modify
the variable interpolation. When you use a variable from Haskell in a Hamlet/Julius file,
it calls `show` on them whenever you want it to appear in the HTML/JS. It would be
better if this also took a user into account.
