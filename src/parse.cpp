#include "parse.h"

#define INTEGER "\"integer\""
#define NUMERIC "\"numeric\""
#define LOGICAL "\"logical\""
#define STRING "\"character\""
#define NIL "null"
#define NILEXP Rcpp::as<SEXP>(R_NilValue)

using ordered_json = nlohmann::ordered_json;

ordered_json mlc_serialize(SEXP x, ordered_json schema);
ordered_json mlc_serialize_list(Rcpp::List data, ordered_json schema);
ordered_json mlc_serialize_dataframe(Rcpp::DataFrame data, ordered_json schema);
SEXP mlc_deserialize_(ordered_json data, ordered_json schema);

SEXP mlc_deserialize_(std::string data, std::string schema){
    return(mlc_deserialize_(ordered_json::parse(data), ordered_json::parse(schema)));
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
    ordered_json schema = ordered_json::parse(schema_str);
    ordered_json output = mlc_serialize(x, schema);
    return (output.dump());
}

ordered_json mlc_serialize(SEXP x, ordered_json schema){
    ordered_json output;

    switch(TYPEOF(x)){
        case NILSXP:
            // the uninitialized json is already null
            break;
        case INTSXP:
            // deal with integer and real types according to the type schema
        case REALSXP:
            if (schema.size() == 1 && schema.contains("list")){
                ordered_json list_type = schema["list"].at(0);
                if (list_type.dump() == NUMERIC){
                    output = Rcpp::as<Rcpp::NumericVector>(x);
                } else if (list_type.dump() == INTEGER){
                    output = Rcpp::as<Rcpp::IntegerVector>(x);
                } else {
                    Rcpp::stop("Found R numeric vector but expected type '%s'", schema.dump());
                }
            } else if (schema.dump() == NUMERIC) {
                output = Rcpp::as<double>(x);
            } else if (schema.dump() == INTEGER) {
                output = Rcpp::as<int>(x);
            } else {
                Rcpp::stop("Found R numeric but expected type '%s'", schema.dump());
            }
            break;
        case STRSXP:
            if (schema.size() == 1 && schema.contains("list") && schema["list"].at(0).dump() == STRING){
                output = Rcpp::as<Rcpp::StringVector>(x);
            } else if (schema.dump() == STRING) {
                output = Rcpp::as<std::string>(x);
            } else {
                Rcpp::stop("Found R character but expected type '%s'", schema.dump());
            }
            break;
        case LGLSXP:
            if (schema.size() == 1 && schema.contains("list") && schema["list"].at(0).dump() == LOGICAL){
                output = Rcpp::as<Rcpp::LogicalVector>(x);
            } else if (schema.dump() == LOGICAL) {
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
ordered_json mlc_serialize_list(Rcpp::List x, ordered_json schema){
    ordered_json output;

    if (! schema.is_object()){
        Rcpp::stop("Expected R list data for type: ", schema.dump());
    }

    if (schema.size() == 1 && schema.contains("tuple")){
        for(R_xlen_t i = 0; i < x.size(); i++){
            ordered_json el = mlc_serialize(x.at(i), schema["tuple"].at(i));
            output.push_back(el);
        }
    } else if (schema.size() == 1 && schema.contains("list")){
        ordered_json list_type = schema["list"].at(0);
        for(Rcpp::List::iterator it = x.begin(); it != x.end(); ++it) {
            ordered_json el = mlc_serialize(*it, list_type);
            output.push_back(el);
        }
    } else if (schema.size() == 1 && schema.contains("record")) {
        output = mlc_serialize_list(x, schema["record"]);
    } else if (schema.size() == 1 && schema.contains("table")) {
        output = mlc_serialize_list(x, schema["table"]);
    } else if (schema.size() == 1 && schema.contains("matrix")){
        Rcpp::stop("R matrix serialization is not yet supported");
    } else {
        output = ordered_json::object();
        // Iterate through the key-val pairs in an object
        for (ordered_json::iterator it = schema.begin(); it != schema.end(); ++it) {
            ordered_json el = mlc_serialize(x[it.key()], it.value());
            output[it.key()] = el;
        }
    }
    return(output);
}


SEXP mlc_deserialize_(ordered_json data, ordered_json schema){
    SEXP result;
    if (schema.is_null()){
        result = NILEXP;
    } else if (schema.is_string()){
        if (schema.dump() == INTEGER){
            Rcpp::IntegerVector x = { data.get<int>() };
            result = Rcpp::as<SEXP>(x);
        } else if (schema.dump() == NUMERIC) {
            Rcpp::NumericVector x = { data.get<double>() };
            result = Rcpp::as<SEXP>(x);
        } else if (schema.dump() == LOGICAL) {
            Rcpp::LogicalVector x = { data.get<bool>() };
            result = Rcpp::as<SEXP>(x);
        } else if (schema.dump() == STRING) {
            Rcpp::StringVector x = { data.get<std::string>() };
            result = Rcpp::as<SEXP>(x);
        } else {
            Rcpp::stop("Could not deserialize JSON data of R type '%s'", schema.dump());
        }
    } else if (schema.size() == 1 && (schema.contains("list"))) {
        ordered_json list_type = schema["list"].at(0);
        if (list_type.dump() == INTEGER){
            std::vector<int> xs = data.get<std::vector<int>>();
            Rcpp::IntegerVector rs = Rcpp::IntegerVector::import(xs.begin(), xs.end());
            result = Rcpp::as<SEXP>(rs);
        } else if (list_type.dump() == NUMERIC) {
            std::vector<double> xs = data.get<std::vector<double>>();
            Rcpp::NumericVector rs = Rcpp::NumericVector::import(xs.begin(), xs.end());
            result = Rcpp::as<SEXP>(rs);
        } else if (list_type.dump() == LOGICAL) {
            std::vector<bool> xs = data.get<std::vector<bool>>();
            Rcpp::LogicalVector rs = Rcpp::LogicalVector::import(xs.begin(), xs.end());
            result = Rcpp::as<SEXP>(rs);
        } else if (list_type.dump() == STRING) {
            std::vector<std::string> xs = data.get<std::vector<std::string>>();
            Rcpp::StringVector rs = Rcpp::StringVector::import(xs.begin(), xs.end());
            result = Rcpp::as<SEXP>(rs);
        } else {
            Rcpp::List rs = Rcpp::List::create();
            for (ordered_json::iterator it = data.begin(); it != data.end(); ++it) {
                SEXP val = mlc_deserialize_(*it, list_type);
                rs.push_back(val);
            }
            result = Rcpp::as<SEXP>(rs);
        }
    } else if (schema.size() == 1 && schema.contains("tuple")){
        Rcpp::List L = Rcpp::List::create();
        ordered_json tuple_types = schema["tuple"];
        if(tuple_types.size() != data.size()){
            Rcpp::stop(
                "Wrong number of elements, expected %d but found %d in tuple of R type: %s",
                tuple_types.size(),
                data.size(),
                schema.dump()
            );
        }
        for (size_t i = 0; i < tuple_types.size(); i++){
            SEXP val = mlc_deserialize_(data.at(i), tuple_types.at(i));
            L.push_back(val);
        }
        result = Rcpp::as<SEXP>(L);
    } else if (schema.size() == 1 && schema.contains("record")){
        result = mlc_deserialize_(data, schema["record"]); 
    } else if (schema.size() == 1 && schema.contains("table")){
        // FIXME: there must be an easier solution ...
        Rcpp::List L = Rcpp::as<Rcpp::List>(mlc_deserialize_(data, schema["table"])); 
        // Solution adapted from https://gallery.rcpp.org/articles/faster-data-frame-creation/
        Rcpp::GenericVector sample_row = L(0);
        Rcpp::StringVector row_names(sample_row.length());
        for (int i = 0; i < sample_row.length(); ++i) {
            char name[15]; // replace with `(floor(log10(N))+1)`
            sprintf(&(name[0]), "%d", i);
            row_names(i) = name;
        }
        // If row.names is not set here, an empty data frame is returned. This
        // seems weird, since row names are generally disfavored in the R
        // community. I will set them to NULL on the R side, I guess.
        L.attr("row.names") = row_names;
        L.attr("class") = "data.frame";
        result = Rcpp::as<SEXP>(L);
    } else if (schema.size() == 1 && schema.contains("matrix")){
        Rcpp::stop("R matrix deserialization is not yet supported");
    } else if (schema.is_object()) {
        Rcpp::List L = Rcpp::List::create();
        for (ordered_json::iterator it = schema.begin(); it != schema.end(); ++it) {
            SEXP el = mlc_deserialize_(data[it.key()], it.value());
            L.push_back(el, it.key());
        }
        result = Rcpp::as<SEXP>(L);
    } else {
        Rcpp::stop("Could not deserialize R data of type: %s", schema.dump());
    }
    return(result);
}
