module Format exposing (float, int, string)


int : String -> Int -> String -> String
int name value input =
    string name (String.fromInt value) input


float : String -> Float -> String -> String
float name value input =
    string name (String.fromFloat value) input


string : String -> String -> String -> String
string name value input =
    String.replace ("{" ++ name ++ "}") value input
