//! Index types. See [`::index_vec`].

pub use index_vec::{
    Idx, IdxRangeBounds, IdxSliceIndex, IndexBox, IndexSlice, IndexVec, index_box, index_vec,
};

/// Creates a new index to use with [`::index_vec`].
#[macro_export]
macro_rules! newtype_index {
    () => {};
    ($(#[$attr:meta])* $vis:vis struct $name:ident; $($rest:tt)*) => {
        $(#[$attr])*
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(transparent)]
        $vis struct $name(std::num::NonZero<u32>);

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({:?})", stringify!($name), self.get())
            }
        }

        impl $crate::index::Idx for $name {
            #[inline(always)]
            fn from_usize(value: usize) -> Self {
                let value = u32::try_from(value).expect("index overflowed");
                Self::new(value)
            }

            #[inline(always)]
            fn index(self) -> usize {
                self.get() as usize
            }
        }

        impl $name {
            /// Creates a new `$name` from the given `value`.
            #[inline(always)]
            $vis const fn new(value: u32) -> Self {
                let inner_repr = value.checked_add(1).expect("index overflowed");
                Self(std::num::NonZero::new(inner_repr).expect("inner_repr should never be zero"))
            }

            /// Gets the underlying index value.
            #[inline(always)]
            $vis const fn get(self) -> u32 {
                self.0.get() - 1
            }
        }

        impl std::ops::Add<u32> for $name {
            type Output = Self;
            fn add(self, rhs: u32) -> Self::Output {
                Self::new(self.get().checked_add(rhs).expect("index addition overflowed"))
            }
        }

        $crate::newtype_index!($($rest)*);
    };
}

newtype_index! {
    pub struct FunctionId;
    pub struct BasicBlockId;
    pub struct OperationIndex;
    pub struct DataOffset;
    pub struct LocalId;
    pub struct LocalIndex;
    pub struct LargeConstId;
    pub struct CasesId;
}

#[cfg(test)]
mod tests {
    newtype_index!(
        struct MyIndex;
    );

    #[test]
    fn base_index() {
        assert_eq!(MyIndex::new(0).get(), 0);
        assert_eq!(MyIndex::new(1).get(), 1);
        assert_eq!(MyIndex::new(0xFFFF_FF00).get(), 0xFFFF_FF00);
    }
}
