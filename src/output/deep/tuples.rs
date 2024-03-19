macro_rules! deep_tuples {
    ($m:ident, $(($k:ident, $t:ident, $i:tt)),+) => {
        mod $m {
            use crate::output::*;

            impl<$($k: Kind),+> Kind for Deep<($($k),+,)>
            {
                type Return = AsReturn<$($k),+,>;
            }

            impl<$($k: Return),+> Return for Deep<($($k),+,)>
            where
                $(<$k as Return>::Type: Send + Sync + 'static),+,
            {
                type Type = ($(<$k as Return>::Type),+,);
            }

            pub struct AsReturn<$($k: Kind),+,>($($k::Return),+,);

            impl<$($k),+> GetOutput for AsReturn<$($k),+,>
            where
                $($k: Kind),+,
            {
                type Output<'u> = ($(<<$k as Kind>::Return as GetOutput>::Output<'u>),+,);

                fn output(&self) -> Option<Self::Output<'_>> {
                    Some(($(self.$i.output()?),+,))
                }
            }

            impl<$($k),+, $($t),+> IntoReturnOnce<Deep<($($k),+,)>> for ($($t),+,)
            where
                $($k: Return),+,
                $(<$k as Return>::Type: 'static + Send + Sync),+,
                $($t: IntoReturnOnce<$k>),+,
            {
                fn into_return_once(self) -> OutputResult<AsReturn<$($k),+,>> {
                    Ok(AsReturn($(self.$i.into_return_once()?),+,))
                }
            }

            impl<$($k),+, $($t),+> IntoReturn<Deep<($($k),+,)>> for ($($t),+,)
            where
                $($k: Return),+,
                $(<$k as Return>::Type: 'static + Send + Sync),+,
                $($t: IntoReturn<$k>),+,
            {
                fn into_return(self) -> OutputResult<AsReturn<$($k),+,>> {
                    Ok(AsReturn($(self.$i.into_return()?),+,))
                }
            }
        }
    };
}

deep_tuples!(tup0, (K0, T0, 0));
deep_tuples!(tup1, (K0, T0, 0), (K1, T1, 1));
deep_tuples!(tup2, (K0, T0, 0), (K1, T1, 1), (K2, T2, 2));
deep_tuples!(tup3, (K0, T0, 0), (K1, T1, 1), (K2, T2, 2), (K3, T3, 3));
