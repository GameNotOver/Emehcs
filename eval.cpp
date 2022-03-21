#include <eval.hpp>
#include <debug.hpp>
#include <exception.hpp>
#include <defs.hpp>

namespace emehcs {

template<typename NumericBinop>
static ValueSharedPtr fold_aux(NumericBinop op, ValueSharedPtr list, ValueSharedPtr res, size_t p) {
    if (p >= list->get<lv::List>().size()) {
        return res;
    }
    res = op(res, list->get<lv::List>()[p]);
    return fold_aux(op, list, res, p + 1);
}

template<typename NumericBinop>
static ValueSharedPtr fold(NumericBinop op, ValueSharedPtr list) {
    ValueSharedPtr res = op(list->get<lv::List>()[1], list->get<lv::List>()[2]);
    return fold_aux(op, list, res, 3);
}

ValueSharedPtr eval(ValueSharedPtr pValue) {
    switch (pValue->get_type()) {
        case LispValType::String:
        case LispValType::Number:
        case LispValType::Bool:
            return pValue;
        case LispValType::List: {
            if (pValue->get<lv::List>().size() > 0 && pValue->get<lv::List>()[0]->get_type() == LispValType::Atom) {
                if (pValue->get<lv::List>()[0]->get<lv::Atom>().str == "quote") {
                    return pValue->get<lv::List>()[1];
                }

                auto func {pValue->get<lv::List>()[0]->get<lv::Atom>()};

                if (pValue->get<lv::List>().size() == 2) {
                    auto fnd = UnaryOps.find(func.str);
                    if (fnd != UnaryOps.cend()) {
                        return fnd->second(pValue->get<lv::List>()[1]);
                    }
                }
                auto fnd{BinaryOps.find(func.str)};
                if (fnd != BinaryOps.cend()) {
                    return fold(fnd->second, pValue);
                }
            }
            if (pValue->get<lv::List>().size() > 0) {
                throw NotFunctionException("[NotFunctionException] Head of a `List` is not a function/functor", pValue->get<lv::List>()[0]);
            }
            else {
                throw BadSpecialFormException("[BadSpecialFormException] A empty `List` without quote", pValue);
            }
        }
        default:
            break;
    }

    throw BadSpecialFormException("[BadSpecialFormException] Unrecognized special form", pValue);
}

ValueSharedPtr numericUnopMinus(ValueSharedPtr a) {
    UNPACK_A(Num);
    return make_shared_value(-a->get<lv::Number>());
}

ValueSharedPtr boolBoolUnopNot(ValueSharedPtr a) {
    UNPACK_A(Bool);
    return make_shared_value(!a->get<lv::Bool>());
}

ValueSharedPtr listCar(ValueSharedPtr a) {
    EVAL_A();
    switch (a->get_type()) {
        case LispValType::List:
            return a->get<lv::List>()[0];
        case LispValType::DottedList:
            return a->get<lv::DottedList>().first.front();
        default:
            throw TypeMismatchException("[TypeMismatchException] Excepted a `list` but received", a);
    }
}

ValueSharedPtr listCdr(ValueSharedPtr a) {
    EVAL_A();
    switch (a->get_type()) {
        case LispValType::List: {
            lv::List cdr {a->get<lv::List>()};
            cdr.pop_front();
            return make_shared_value(cdr);
        }
        case LispValType::DottedList: {
            if (a->get<lv::DottedList>().first.size() == 1) {
                return a->get<lv::DottedList>().second;
            }
            lv::List first_cdr {a->get<lv::DottedList>().first};
            first_cdr.pop_front();
            return make_shared_value(lv::DottedList(first_cdr, a->get<lv::DottedList>().second));
        }
        default:
            throw TypeMismatchException("[TypeMismatchException] Excepted a `list` but received", a);
    }
}

ValueSharedPtr numericBinopPlus(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() + b->get<lv::Number>());
}

ValueSharedPtr numericBinopMinus(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() - b->get<lv::Number>());
}

ValueSharedPtr numericBinopTimes(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() * b->get<lv::Number>());
}

ValueSharedPtr numericBinopDivide(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() / b->get<lv::Number>());
}

ValueSharedPtr numericBinopMod(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    lv::Number aa = a->get<lv::Number>(), bb = b->get<lv::Number>();
    if (bb > 0) {
        return make_shared_value(aa % bb);
    }
    else {
        return make_shared_value((aa * bb < 0 ? -1 : 1) * (-bb - ::emehcs::abs(aa) % ::emehcs::abs(bb)));
    }
}

ValueSharedPtr numericBinopQuot(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() / b->get<lv::Number>());
}

ValueSharedPtr numericBinopRem(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() % b->get<lv::Number>());
}

ValueSharedPtr numBoolBinopEq(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() == b->get<lv::Number>());
}

ValueSharedPtr numBoolBinopL(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() < b->get<lv::Number>());
}

ValueSharedPtr numBoolBinopLe(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() <= b->get<lv::Number>());
}

ValueSharedPtr numBoolBinopG(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() > b->get<lv::Number>());
}

ValueSharedPtr numBoolBinopGe(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() >= b->get<lv::Number>());
}

ValueSharedPtr numBoolBinopNeq(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Num);
    return make_shared_value(a->get<lv::Number>() != b->get<lv::Number>());
}

ValueSharedPtr boolBoolBinopAnd(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Bool);
    return make_shared_value(a->get<lv::Bool>() && b->get<lv::Bool>());
}

ValueSharedPtr boolBoolBinopOr(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Bool);
    return make_shared_value(a->get<lv::Bool>() || b->get<lv::Bool>());
}

ValueSharedPtr strBoolBinopEq(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Str);
    return make_shared_value(a->get<lv::String>() == b->get<lv::String>());
}

ValueSharedPtr strBoolBinopL(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Str);
    return make_shared_value(a->get<lv::String>() < b->get<lv::String>());
}

ValueSharedPtr strBoolBinopLe(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Str);
    return make_shared_value(a->get<lv::String>() <= b->get<lv::String>());
}

ValueSharedPtr strBoolBinopG(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Str);
    return make_shared_value(a->get<lv::String>() > b->get<lv::String>());
}

ValueSharedPtr strBoolBinopGe(ValueSharedPtr a, ValueSharedPtr b) {
    UNPACK_AB(Str);
    return make_shared_value(a->get<lv::String>() >= b->get<lv::String>());
}

ValueSharedPtr listCons(ValueSharedPtr a, ValueSharedPtr b) {
    EVAL_AB();
    switch (b->get_type()) {
        case LispValType::List:
            if (b->get<lv::List>().empty()) {
                return make_shared_value(lv::List(1, a));
            }
            else {
                lv::List xs;
                xs.push_back(a);
                for (auto&& x : b->get<lv::List>()) {
                    xs.push_back(x);
                }
                return make_shared_value(xs);
            }
        case LispValType::DottedList: {
            lv::List xs;
            xs.push_back(a);
            for (auto&& x : b->get<lv::DottedList>().first) {
                xs.push_back(x);
            }
            return make_shared_value(lv::DottedList(xs, b->get<lv::DottedList>().second));
        }
        default:
            return make_shared_value(lv::DottedList(lv::List(1, a), b));
    }
}

static ValueSharedPtr eqv_aux(ValueSharedPtr, ValueSharedPtr);

static ValueSharedPtr eqv_aux(const lv::List& a, const lv::List& b) {
    bool ret = true;
    if (a.size() != b.size()) {
        ret = false;
    }
    else {
        const auto len = a.size();
        for (size_t i = 0; i < len; ++i) {
            if (!eqv_aux(a[i], b[i])->get<lv::Bool>()) {
                ret = false;
                break;
            }
        }
    }
    return make_shared_value(ret);
}

ValueSharedPtr eqv_aux(ValueSharedPtr a, ValueSharedPtr b) {
    bool ret = false;

    if (a->get_type() != b->get_type()) {
        throw TypeMismatchException("[TypeMismatchException] Can't compare with different types, the second one is");
    }
    switch (a->get_type()) {
        case LispValType::Bool:
            ret = (a->get<lv::Number>() == b->get<lv::Number>());
            break;
        case LispValType::Number:
            ret = (a->get<lv::Number>() == b->get<lv::Number>());
            break;
        case LispValType::String:
            ret = (a->get<lv::String>() == b->get<lv::String>());
            break;
        case LispValType::Atom:
            ret = (a->get<lv::Atom>().str == b->get<lv::Atom>().str);
            break;
        case LispValType::DottedList:
            ret = boolBoolBinopAnd(eqv_aux(a->get<lv::DottedList>().first, b->get<lv::DottedList>().first),
                                   eqv_aux(a->get<lv::DottedList>().second, b->get<lv::DottedList>().second))->get<lv::Bool>();
            break;
        case LispValType::List:
            ret = eqv_aux(a->get<lv::List>(), b->get<lv::List>())->get<lv::Bool>();
            break;
        default:
            ret = false;
            break;
    }

    return make_shared_value(ret);
}

ValueSharedPtr eqv(ValueSharedPtr a, ValueSharedPtr b) {
    EVAL_AB();
    return eqv_aux(a, b);
}

}
