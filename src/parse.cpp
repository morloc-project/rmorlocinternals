#include "parse.h"

#define INTEGER "\"integer\""_json
#define NUMERIC "\"numeric\""_json
#define LOGICAL "\"logical\""_json
#define STRING "\"character\""_json
#define NIL "null"_json
#define NILEXP Rcpp::as<SEXP>(R_NilValue)

using json = nlohmann::json;

json mlc_serialize(SEXP x, json schema);
json mlc_serialize_list(Rcpp::List data, json schema);
json mlc_serialize_dataframe(Rcpp::DataFrame data, json schema);
SEXP mlc_deserialize(json data, json schema);

SEXP mlc_deserialize(std::string data, std::string schema){
    return(mlc_deserialize(json::parse(data), json::parse(schema)));
}

// SEXP types, from https://cran.r-project.org/doc/manuals/r-release/R-ints.html#SEXPs
// 0     NILSXP      NULL
// 1     SYMSXP      symbols
// 2     LISTSXP     pairlists
// 3     CLOSXP      closures
// 4     ENVSXP      environments
// 5     PROMSXP     promises
// 6     LANGSXP     language objects
// 7     SPECIALSXP  special functions
// 8     BUILTINSXP  builtin functions
// 9     CHARSXP     internal character strings
// 10    LGLSXP      logical vectors
// 13    INTSXP      integer vectors
// 14    REALSXP     numeric vectors
// 15    CPLXSXP     complex vectors
// 16    STRSXP      character vectors
// 17    DOTSXP      dot-dot-dot object
// 18    ANYSXP      make “any” args work
// 19    VECSXP      list (generic vector)
// 20    EXPRSXP     expression vector
// 21    BCODESXP    byte code
// 22    EXTPTRSXP   external pointer
// 23    WEAKREFSXP  weak reference
// 24    RAWSXP      raw vector
// 25    S4SXP       S4 classes not of simple type


Rcpp::String mlc_serialize(SEXP x, std::string schema_str){
    json schema = json::parse(schema_str);
    json output = mlc_serialize(x, schema);
    return (output.dump());
}

json mlc_serialize(SEXP x, json schema){
    json output;

    switch(TYPEOF(x)){
        case NILSXP:
            // the uninitialized json is already null
            break;
        case INTSXP:
            // deal with integer and real types according to the type schema
        case REALSXP:
            if (schema.size() == 1 && schema.contains("list")){
                json list_type = schema["list"].at(0);
                if (list_type == NUMERIC){
                    output = Rcpp::as<Rcpp::NumericVector>(x);
                } else if (list_type == INTEGER){
                    output = Rcpp::as<Rcpp::IntegerVector>(x);
                } else {
                    Rcpp::stop("Found R numeric vector but expected type '%s'", schema.dump());
                }
            } else if (schema == NUMERIC) {
                output = Rcpp::as<double>(x);
            } else if (schema == INTEGER) {
                output = Rcpp::as<int>(x);
            } else {
                Rcpp::stop("Found R numeric but expected type '%s'", schema.dump());
            }
            break;
        case STRSXP:
            if (schema.size() == 1 && schema.contains("list") && schema["list"].at(0) == STRING){
                output = Rcpp::as<Rcpp::StringVector>(x);
            } else if (schema == STRING) {
                output = Rcpp::as<std::string>(x);
            } else {
                Rcpp::stop("Found R character but expected type '%s'", schema.dump());
            }
            break;
        case LGLSXP:
            if (schema.size() == 1 && schema.contains("list") && schema["list"].at(0) == LOGICAL){
                output = Rcpp::as<Rcpp::LogicalVector>(x);
            } else if (schema == LOGICAL) {
                output = Rcpp::as<bool>(x);
            } else {
                Rcpp::stop("Found R logical but expected type '%s'", schema.dump());
            }
            break;
        case VECSXP:
            output = mlc_serialize_list(Rcpp::as<Rcpp::List>(x), schema);
            break;
        default:
            Rcpp::stop("Could not serialize data of type '%s'", schema.dump());
            break;
    }

    return output;
}

// serialize(list(1,2), '{"tuple":["integer", "integer"]}')
json mlc_serialize_list(Rcpp::List x, json schema){
    json output;
    if (schema.contains("tuple")){
        for(R_xlen_t i = 0; i < x.size(); i++){
            json el = mlc_serialize(x.at(i), schema["tuple"].at(i));
            output.push_back(el);
        }
    } else if (schema.contains("list")){
        json list_type = schema["list"].at(0);
        for(Rcpp::List::iterator it = x.begin(); it != x.end(); ++it) {
            json el = mlc_serialize(*it, list_type);
            output.push_back(el);
        }
    } else {
        output = json::object();
        // Iterate through the
        for (json::iterator it = schema.begin(); it != schema.end(); ++it) {
            json el = mlc_serialize(x[it.key()], it.value());
            output[it.key()] = el;
        }
    }
    return(output);
}


json mlc_serialize_dataframe(Rcpp::DataFrame, json schema){
    json j("dataFrame");
    return(j);
}

SEXP mlc_deserialize(json data, json schema){
    // FIXME: This is grossly inefficient. What we SHOULD do is convert the
    // schema into a structure of enums (e.g., [INT, DOUBLE, BOOL, LIST, TUPLE,
    // ...])

    SEXP result;

    if (schema.is_string()){
        if (schema == INTEGER){
            Rcpp::IntegerVector x = { data.get<int>() };
            result = Rcpp::as<SEXP>(x);
        } else if (schema == NUMERIC) {
            Rcpp::NumericVector x = { data.get<double>() };
            result = Rcpp::as<SEXP>(x);
        } else if (schema == LOGICAL) {
            Rcpp::LogicalVector x = { data.get<bool>() };
            result = Rcpp::as<SEXP>(x);
        } else if (schema == STRING) {
            Rcpp::StringVector x = { data.get<std::string>() };
            result = Rcpp::as<SEXP>(x);
        } else {
            Rcpp::Rcerr << "Unrecognized R value type: '" << schema.dump()
                        << "' returning NULL" << std::endl;
            result = Rcpp::as<SEXP>(R_NilValue);
        }
    } else if (schema.is_null()){
        result = Rcpp::as<SEXP>(R_NilValue);
    } else {
        if (schema.contains("list")){
            json list_type = schema["list"].at(0);
            if (list_type == INTEGER){
                std::vector<int> xs = data.get<std::vector<int>>();
                Rcpp::IntegerVector rs = Rcpp::IntegerVector::import(xs.begin(), xs.end());
                result = Rcpp::as<SEXP>(rs);
            } else if (list_type == NUMERIC) {
                std::vector<double> xs = data.get<std::vector<double>>();
                Rcpp::NumericVector rs = Rcpp::NumericVector::import(xs.begin(), xs.end());
                result = Rcpp::as<SEXP>(rs);
            } else if (list_type == LOGICAL) {
                std::vector<bool> xs = data.get<std::vector<bool>>();
                Rcpp::LogicalVector rs = Rcpp::LogicalVector::import(xs.begin(), xs.end());
                result = Rcpp::as<SEXP>(rs);
            } else if (list_type == STRING) {
                std::vector<std::string> xs = data.get<std::vector<std::string>>();
                Rcpp::StringVector rs = Rcpp::StringVector::import(xs.begin(), xs.end());
                result = Rcpp::as<SEXP>(rs);
            } else {
                Rcpp::List L = Rcpp::List::create();
                for (json::iterator it = data.begin(); it != data.end(); ++it) {
                    SEXP val = mlc_deserialize(*it, list_type);
                    L.push_back(val);
                }
                result = Rcpp::as<SEXP>(L);
            }
        } else if (schema.contains("tuple")){
            Rcpp::List L = Rcpp::List::create();
            json tuple_types = schema["tuple"];
            for (size_t i = 0; i < tuple_types.size(); i++){
                SEXP val = mlc_deserialize(data.at(i), tuple_types.at(i));
                L.push_back(val);
            }
            result = Rcpp::as<SEXP>(L);
        } else {
            Rcpp::Rcerr << "Unrecognized R object type: " << schema.dump() << std::endl;
        }
    }
    return(result);
}
