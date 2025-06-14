pub mod declaration;
pub mod definition;

#[cfg(test)]
mod tests {
    use crate::semantic::test_util::{assert_is_err, assert_is_ok};
    use indoc::indoc;

    #[test]
    fn empty_record() {
        assert_is_ok(indoc! {"
            record User {}
        "});
    }

    #[test]
    fn record_with_multiple_fields() {
        assert_is_ok(indoc! {"
            record User {
                name: string,
                age: int,
            }
        "});
    }

    #[test]
    fn record_with_compound_type_fields() {
        assert_is_ok(indoc! {"
            record User {
                friends: []int,
                greet: () -> void,
            }
        "});
    }

    #[test]
    fn record_with_recursive_type() {
        assert_is_ok(indoc! {"
            record User {
                supervisor: User,
            }
        "});
    }

    #[test]
    fn multiple_record_types_with_recursive_relation() {
        assert_is_ok(indoc! {"
            record User {
                groups: []Group,
            }

            record Group {
                users: []User,
            }
        "});
    }

    #[test]
    fn record_as_function_parameter() {
        assert_is_ok(indoc! {"
            def foo(user: User) {}

            record User {}
        "});
    }

    #[test]
    fn record_with_duplicated_names() {
        assert_is_err(indoc! {"
            record User {}
            record User {}
        "});
    }

    #[test]
    fn record_with_duplicated_fields() {
        assert_is_err(indoc! {"
            record User {
                age: int,
                age: sbyte,
            }
        "});
    }
}
