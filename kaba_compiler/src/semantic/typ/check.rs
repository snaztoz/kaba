use crate::semantic::{
    state::AnalyzerState,
    typ::{FloatType, IntType},
};

use super::Type;

pub fn are_types_compatible(a: &Type, b: &Type) -> bool {
    if a == b {
        return true;
    }

    if a.is_unbounded_int() && b.is_bounded_int() {
        return is_unbounded_number_bindable_to(a, b);
    } else if b.is_unbounded_int() && a.is_bounded_int() {
        return is_unbounded_number_bindable_to(b, a);
    }

    if a.is_unbounded_float() && b.is_bounded_float() {
        return is_unbounded_number_bindable_to(a, b);
    } else if b.is_unbounded_float() && a.is_bounded_float() {
        return is_unbounded_number_bindable_to(b, a);
    }

    false
}

pub fn is_type_assignable_to(t: &Type, to: &Type, state: &AnalyzerState) -> bool {
    let t = if let Type::Symbol(name) = t {
        state.get_sym_variant(name).unwrap().as_type_t()
    } else {
        t
    };

    let to = if let Type::Symbol(name) = to {
        state.get_sym_variant(name).unwrap().as_type_t()
    } else {
        to
    };

    if t == to {
        return true;
    }

    if t.is_record() && to.is_record() {
        let t_fields = t.as_record_fields();
        let to_fields = to.as_record_fields();

        if t_fields.len() != to_fields.len() {
            return false;
        }

        return t_fields
            .iter()
            .all(|(field_name, field_t)| match to_fields.get(field_name) {
                Some(to_field_t) => is_type_assignable_to(field_t, to_field_t, state),

                None => false,
            });
    }

    if t.is_unbounded_int() || t.is_unbounded_float() {
        return is_unbounded_number_bindable_to(t, to);
    }

    false
}

fn is_unbounded_number_bindable_to(unbounded_t: &Type, target: &Type) -> bool {
    match unbounded_t {
        Type::Int(IntType::Unbounded(n)) => match target {
            // Try narrowing the value
            Type::Int(IntType::SByte) => i8::try_from(*n).is_ok(),
            Type::Int(IntType::Short) => i16::try_from(*n).is_ok(),

            // Will always fit; does not need any checking
            Type::Int(IntType::Int) | Type::Int(IntType::Long) => true,

            _ => false,
        },

        Type::Float(FloatType::Unbounded(_)) => match target {
            // Always fit, does not need any checking
            Type::Float(FloatType::Float) | Type::Float(FloatType::Double) => true,

            _ => false,
        },

        _ => unreachable!(),
    }
}
