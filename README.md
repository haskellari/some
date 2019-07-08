some
====

This library defines an existential type 'Some'.

    data Some f where
        Some :: f a -> Some f

and operations to work with it.
