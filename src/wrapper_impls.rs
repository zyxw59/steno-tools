#[macro_export]
macro_rules! fmt_impls {
    ($ty:ty) => {
        impl ::std::fmt::Display for $ty {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                ::std::fmt::Display::fmt(&self.0, f)
            }
        }
        impl ::std::fmt::Debug for $ty {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                ::std::fmt::Debug::fmt(&self.0, f)
            }
        }
    };
}

#[macro_export]
macro_rules! deref_impls {
    ($ty:ty as $deref:ty) => {
        impl ::std::ops::Deref for $ty {
            type Target = $deref;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
        impl ::std::borrow::Borrow<$deref> for $ty {
            fn borrow(&self) -> &$deref {
                &self.0
            }
        }
    };
}
