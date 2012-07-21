#define Q_OBJECT int foo;
#define Q_DECLARE_PRIVATE(obj) int obj;
class Foo : public QObject
{
    Q_OBJECT
    Q_DECLARE_PRIVATE(QObject)
public:
    Foo();
};
/* Local Variables: */
/* c-macro-names-with-semicolon: ("Q_OBJECT" "Q_DECLARE_PRIVATE") */
/* eval: (c-make-macro-with-semi-re) */
/* End: */
