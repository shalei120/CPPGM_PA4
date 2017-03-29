// (C) 2013 CPPGM Foundation www.cppgm.org.  All rights reserved.

#include <iostream>
#include <sstream>
#include <fstream>
#include <stdexcept>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <cassert>
#include <memory>
#include <cstring>
#include <cstdint>
#include <climits>
#include <map>
#include <stack>
#include <set>
#include <iterator>

#include "IPPTokenStream.h"

using namespace std;
// Tokenizer
struct PA1_PPTokenizer
{
	IPPTokenStream& output;

	string delimiter="",delimiter2="";

	PA1_PPTokenizer(IPPTokenStream& output)
		: output(output)
	{}
int state=0;
	

	bool process(int c, int pos)       //return whether or not the pointer should go back one int
	{
		// TODO:  Your code goes here.

		// 1. do translation features
		// 2. tokenize resulting stream
		// 3. call an output.emit_* function for each token.
		//static int state=0;
		static string result;
		static bool header = false, headerstart = true;
		static int escapefrom = -1, blankfrom = -1;
		//cout<<state<<" "<<headerstart<<" "<<header<<" "<<delimiter<<" "<<delimiter2<<" "<<char(c)<<endl;
		switch(state){
			case 0: //result = ""; escapefrom = -1;
				if(c == ' ' || c == '\t') state = 1;
				else if(Is_identifier_nondigit(c) && !Is_Initial_Banned(c) && c != 'u' && c != 'U' && c != 'L' && c != 'R') {
					state = 2; 
					result += Unicode2Utf8(c);					
				}
				else if(Digit.find(c) != Digit.end()) {state = 3; result += Unicode2Utf8(c);}
				else if(c == '.') {state = 5; result += Unicode2Utf8(c);}
				else if(c == '\''){state = 7; result += Unicode2Utf8(c);}
				else if(c == 'U' || c == 'L'){state = 10; result += Unicode2Utf8(c);}
				else if(c == 'u'){state = 11; result += Unicode2Utf8(c);}
				else if(c == '\"'){state = 13; result += Unicode2Utf8(c);}
				else if(c == 'R'){state = 16; result += Unicode2Utf8(c);}
				else if(c == '{'){output.emit_preprocessing_op_or_punc("{"); headerstart = false;}
				else if(c == '}'){output.emit_preprocessing_op_or_punc("}"); headerstart = false;}
				else if(c == '['){output.emit_preprocessing_op_or_punc("["); headerstart = false;}
				else if(c == ']'){output.emit_preprocessing_op_or_punc("]"); headerstart = false;}
				else if(c == '('){output.emit_preprocessing_op_or_punc("("); headerstart = false;}
				else if(c == ')'){output.emit_preprocessing_op_or_punc(")"); headerstart = false;}
				else if(c == ';'){output.emit_preprocessing_op_or_punc(";"); headerstart = false;}
				else if(c == '?'){output.emit_preprocessing_op_or_punc("?"); headerstart = false;}
				else if(c == '~'){output.emit_preprocessing_op_or_punc("~"); headerstart = false;}
				else if(c == ','){output.emit_preprocessing_op_or_punc(","); headerstart = false;}
				else if(c == '\n'){output.emit_new_line(); headerstart = true;header=false;}
				else if(c == '#'){state = 17;result += Unicode2Utf8(c);}
				else if(c == '<'){state = 18;result += Unicode2Utf8(c);}
				else if(c == ':'){state = 22;result += Unicode2Utf8(c);}
				else if(c == '%'){state = 23;result += Unicode2Utf8(c);}
				else if(c == '+'){state = 26;result += Unicode2Utf8(c);}
				else if(c == '-'){state = 27;result += Unicode2Utf8(c);}
				else if(c == '=' || c == '!' || c == '*' || c == '^'){state = 29;result += Unicode2Utf8(c);}
				else if(c == '/'){state = 46;result += Unicode2Utf8(c);blankfrom = 0;}
				else if(c == '&'){state = 30;result += Unicode2Utf8(c);}
				else if(c == '|'){state = 31;result += Unicode2Utf8(c);}
				else if(c == '>'){state = 32;result += Unicode2Utf8(c);}
				else if(c == EndOfFile)break;
				else{result += Unicode2Utf8(c); output.emit_non_whitespace_char(result); result = ""; headerstart = false;}
				break;
			case 1:if(c == ' ' || c == 0x0d || c == '\t') state = 1;
				else if(c == '/'){state = 51;blankfrom = 0;}
				else{state = 0; output.emit_whitespace_sequence(); return 1;}
				break;
			case 2:if(Is_identifier_nondigit(c) || Digit.find(c) != Digit.end()) {state = 2; result += Unicode2Utf8(c);}
				else{ 
					state = 0; 
					if(Digraph_IdentifierLike_Operators.find(result) != Digraph_IdentifierLike_Operators.end()){
						output.emit_preprocessing_op_or_punc(result);
					}
					else{
						output.emit_identifier(result); 
						if(result == "include"){
							if(header == true){
								state = 40;
								header = false;
							}
						}else{
							header = false;
						}
					}
					result = ""; 
					headerstart = false;
					return 1;
				}
				break;
			case 3:if((Digit.find(c) != Digit.end() || Is_identifier_nondigit(c) || c == '.') && c != 'e' && c != 'E') {state = 3; result += Unicode2Utf8(c);}
				else if(c == 'e' || c == 'E') {state = 4; result += Unicode2Utf8(c);}
				else{ state = 0; output.emit_pp_number(result); result = ""; headerstart = false; return 1;}
				break;
			case 4:if(Digit.find(c) != Digit.end() || Is_identifier_nondigit(c) || c == '.' || c == '+' || c == '-')
					{state = 3; result += Unicode2Utf8(c);}
				else{ state = 0; output.emit_pp_number(result); result = ""; headerstart = false; return 1;}
				break; 
			case 5:if(Digit.find(c) != Digit.end()){state = 3; result += Unicode2Utf8(c);}
				else if(c == '.'){state = 6; result += Unicode2Utf8(c);}
				else if(c == '*'){state = 0; result += Unicode2Utf8(c); output.emit_preprocessing_op_or_punc(result); result="";}
				else{state = 0; output.emit_preprocessing_op_or_punc(result); result=""; headerstart = false; return 1;}
				break;
			case 6:if(c == '.'){state = 0; result += '.'; output.emit_preprocessing_op_or_punc(result); result = "";}
				else if(Digit.find(c) != Digit.end()){state = 3; output.emit_preprocessing_op_or_punc("."); result = "."; result += Unicode2Utf8(c);}
				else{state = 0; result = ""; output.emit_preprocessing_op_or_punc("."); output.emit_preprocessing_op_or_punc("."); headerstart = false;return 1;}
				break;
			case 7:if(Is_cchar(c)){state = 7; result +=  Unicode2Utf8(c);}
				else if(c == '\\'){state = 34; escapefrom = 7; result += '\\';}
				else if(c == '\''){state = 8; result += '\'';}
				else {cout<<"state 7:unterminated character literal"<<endl;throw logic_error("state 7:unterminated character literal");}
				break;
			case 8:if(Is_identifier_nondigit(c) && !Is_Initial_Banned(c)){state = 9; result += Unicode2Utf8(c);}
				else{state = 0; output.emit_character_literal(result); result = ""; headerstart = false; return 1;}
				break;
			case 9:if(Is_identifier_nondigit(c) || Digit.find(c) != Digit.end()){state = 9; result += Unicode2Utf8(c);}
				else{state = 0; output.emit_user_defined_character_literal(result); result = ""; headerstart = false; return 1;}
				break;
			case 10:if(c == '\''){state = 7; result += Unicode2Utf8(c);}
				else if(c == '\"'){state = 13; result += Unicode2Utf8(c);}
				else if(c == 'R'){state = 16; result += Unicode2Utf8(c);}
				else if(Is_identifier_nondigit(c) || Digit.find(c) != Digit.end()){state = 2; result += Unicode2Utf8(c);}
				else{state = 0; output.emit_identifier(result); result = ""; headerstart = false; return 1;}
				break;
			case 11:if(c == '\''){state = 7; result += Unicode2Utf8(c);}
				else if(c == '\"'){state = 13; result += Unicode2Utf8(c);}
				else if(c == 'R'){state = 16; result += Unicode2Utf8(c);}
				else if(c == '8'){state = 12; result += Unicode2Utf8(c);}
				else if(Is_identifier_nondigit(c) || Digit.find(c) != Digit.end()){state = 2; result += Unicode2Utf8(c);}				
				else{state = 0; output.emit_identifier(result); result = ""; headerstart = false; return 1;}
				break;
			case 12:if(c == '\"'){state = 13; result += Unicode2Utf8(c);}
				else if(c == 'R'){state = 16; result += Unicode2Utf8(c);}
				else if(Is_identifier_nondigit(c) || Digit.find(c) != Digit.end()){state = 2; result += Unicode2Utf8(c);}
				else{state = 0; output.emit_identifier(result); result = ""; headerstart = false; return 1;}
				break;
			case 13:if(Is_schar(c)){state = 13; result +=  Unicode2Utf8(c);}
				else if(c == '\\'){state = 34; escapefrom = 13; result += Unicode2Utf8(c);}
				else if(c == '\"'){state = 14; result += Unicode2Utf8(c);}
				else {cout<<"state 13:unterminated string literal"<<endl;throw logic_error("state 13:unterminated string literal");}
				break;
			case 14:if(Is_identifier_nondigit(c) && !Is_Initial_Banned(c)){state = 15; result += Unicode2Utf8(c);}
				else{state = 0; output.emit_string_literal(result); result = ""; headerstart = false; return 1;}
				break;
			case 15:if(Is_identifier_nondigit(c) || Digit.find(c) != Digit.end()){state = 15; result += Unicode2Utf8(c);}
				else{state = 0; output.emit_user_defined_string_literal(result); result = ""; headerstart = false; return 1;}
				break;
			case 16:if(c == '\"'){state = 47;result += Unicode2Utf8(c);}
				else if(Is_identifier_nondigit(c) || Digit.find(c) != Digit.end()){state = 2; result += Unicode2Utf8(c);}
				else{state = 0; output.emit_identifier(result); result = ""; headerstart = false; return 1;}
				break;
			case 17:if(c == '#'){state = 0; result = ""; output.emit_preprocessing_op_or_punc("##"); headerstart = false;}
				else{if(headerstart == true)header = true; state = 0; result = ""; output.emit_preprocessing_op_or_punc("#"); headerstart = false; return 1;}
				break;
			case 18:if(c == '<'){state = 19; result += Unicode2Utf8(c);}
				else if(c == ':'){state = 20; result += Unicode2Utf8(c);}
				else if(c == '%' || c == '='){state = 0; result += Unicode2Utf8(c); output.emit_preprocessing_op_or_punc(result); result = ""; headerstart = false;}
				else{state = 0; output.emit_preprocessing_op_or_punc("<"); result = ""; headerstart = false; return 1;}
				break;
			case 19:if(c == '='){state = 0; result = ""; output.emit_preprocessing_op_or_punc("<<="); headerstart = false;}
				else{state = 0; result = ""; output.emit_preprocessing_op_or_punc("<<"); headerstart = false; return 1;}
				break;
			case 20:if(c == ':'){state = 21; result += Unicode2Utf8(c);}
				else{state = 0; output.emit_preprocessing_op_or_punc("<:"); result = ""; headerstart = false; return 1;}
				break;
			case 21:if(c == '>'){state = 0; result = ""; output.emit_preprocessing_op_or_punc("<:"); output.emit_preprocessing_op_or_punc(":>"); headerstart = false;}
				else if(c == ':'){state = 0;  result = ""; output.emit_preprocessing_op_or_punc("<:"); output.emit_preprocessing_op_or_punc("::"); headerstart = false;}
				else{state = 0; output.emit_preprocessing_op_or_punc("<"); output.emit_preprocessing_op_or_punc("::"); result = ""; headerstart = false; return 1;}
				break;
			case 22:if(c == ':'){state = 0; result = ""; output.emit_preprocessing_op_or_punc("::"); headerstart = false;}
				else if(c == '>'){state = 0; result = ""; output.emit_preprocessing_op_or_punc(":>"); headerstart = false;}
				else{state = 0; result = ""; output.emit_preprocessing_op_or_punc(":"); headerstart = false; return 1;}
				break;
			case 23:if(c == ':'){state = 24; result += Unicode2Utf8(c);}
				else if(c == '>' || c == '='){state = 0; result += Unicode2Utf8(c); output.emit_preprocessing_op_or_punc(result); result = ""; headerstart = false;}
				else{state = 0; result = ""; output.emit_preprocessing_op_or_punc("%"); headerstart = false; return 1;}
				break;
			case 24:if(c == '%'){state = 25; result += Unicode2Utf8(c);}
				else{state = 0; result = ""; output.emit_preprocessing_op_or_punc("%:"); if( headerstart == true) header = true; headerstart = false; return 1;}
				break;
			case 25:if(c == ':'){state = 0; result = ""; output.emit_preprocessing_op_or_punc("%:%:"); headerstart = false;}
				else{state = 0; result = ""; output.emit_preprocessing_op_or_punc("%:"); output.emit_preprocessing_op_or_punc("%"); headerstart = false; return 1;}
				break;
			case 26:if(c == '+' || c == '='){state = 0; result += Unicode2Utf8(c); output.emit_preprocessing_op_or_punc(result);result = ""; headerstart = false;}
				else{state = 0; result = ""; output.emit_preprocessing_op_or_punc("+"); headerstart = false;return 1;}
				break;
			case 27:if(c == '-' || c == '='){state = 0; result += Unicode2Utf8(c); output.emit_preprocessing_op_or_punc(result);result = ""; headerstart = false;}
				else if(c == '>'){state = 28; result += Unicode2Utf8(c);}
				else{state = 0; result = ""; output.emit_preprocessing_op_or_punc("-"); headerstart = false;return 1;}
				break;
			case 28:if(c == '*'){state = 0; result = ""; output.emit_preprocessing_op_or_punc("->*"); headerstart = false;}
				else{state = 0; result = ""; output.emit_preprocessing_op_or_punc("->"); headerstart = false;return 1;}
				break;
			case 29:if(c == '='){state = 0; result += Unicode2Utf8(c); output.emit_preprocessing_op_or_punc(result); result = ""; headerstart = false;}
				else{state = 0; output.emit_preprocessing_op_or_punc(result); result = ""; headerstart = false; return 1;}
				break;
			case 30:if(c == '=' || c == '&'){state = 0; result += Unicode2Utf8(c); output.emit_preprocessing_op_or_punc(result); result = ""; headerstart = false;}
				else{state = 0; output.emit_preprocessing_op_or_punc("&"); result = ""; headerstart = false; return 1;}
				break;
			case 31:if(c == '=' || c == '|'){state = 0; result += Unicode2Utf8(c); output.emit_preprocessing_op_or_punc(result); result = ""; headerstart = false;}
				else{state = 0; output.emit_preprocessing_op_or_punc("|"); result = ""; headerstart = false; return 1;}
				break;
			case 32:if(c == '>'){state = 33; result += Unicode2Utf8(c); }
				else if(c == '='){state = 0; result = ""; output.emit_preprocessing_op_or_punc(">="); headerstart = false;}
				else{state = 0; output.emit_preprocessing_op_or_punc(">"); result = ""; headerstart = false; return 1;}
				break;
			case 33:if(c == '='){state = 0; result = ""; output.emit_preprocessing_op_or_punc(">>="); headerstart = false;}
				else{state = 0; output.emit_preprocessing_op_or_punc(">>"); result = ""; headerstart = false; return 1;}
				break;
			case 34:if(SimpleEscapeSequence_CodePoints.find(c) != SimpleEscapeSequence_CodePoints.end()){					
					result += Unicode2Utf8(c);
					state = escapefrom;
					escapefrom = -1;
				}
				else if(Octal_Digit.find(c) != Octal_Digit.end()){state = 35; result += Unicode2Utf8(c);}
				else if(c == 'x'){state = 38; result += Unicode2Utf8(c);}
				else {cout<<"invalid escape sequence\n";throw logic_error("invalid escape sequence\n");}
				break;
			case 35:if(Octal_Digit.find(c) != Octal_Digit.end()){state = 36; result += Unicode2Utf8(c);}
				else{state = escapefrom; escapefrom = -1; return 1;}
				break;
			case 36:if(Octal_Digit.find(c) != Octal_Digit.end()){state = escapefrom; escapefrom = -1; result += Unicode2Utf8(c);}
				else{state = escapefrom; escapefrom = -1; return 1;}
				break;
			case 37:if(c==0x0a || c==-1){
					if(blankfrom == 0)state=0;
					else if(blankfrom == 41)state = 41;
					else throw logic_error("invalid blankfrom");
					blankfrom = -1; 
					output.emit_whitespace_sequence();
					output.emit_new_line();
					result = "";
				}
				else {state = 37;}
				break;
			case 38:if(Hexadecimal_Digit.find(c) != Hexadecimal_Digit.end()){state = 39; result += Unicode2Utf8(c);}
				else {cout<<"invalid escape sequence\n";throw logic_error("invalid escape sequence\n");}
				break;
			case 39:if(Hexadecimal_Digit.find(c) != Hexadecimal_Digit.end()){state = 39; result += Unicode2Utf8(c);}
				else{state = escapefrom; escapefrom = -1; return 1;}
				break;
			case 40:if(c == ' '){state = 41;}
				else if(c == '<'){state = 42; result += Unicode2Utf8(c);}
				else if(c == '\"'){state = 43; result += Unicode2Utf8(c);}
				else if(c == '/'){state = 51; blankfrom = 41;}
				else{state = 0; return 1;}
				break;
			case 41:if(c == ' '){state = 41;}
				else if(c == '<'){state = 42; result += Unicode2Utf8(c); output.emit_whitespace_sequence();}
				else if(c == '\"'){state = 43; result += Unicode2Utf8(c); output.emit_whitespace_sequence();}
				else if(c == '/'){state = 51; blankfrom = 41;}
				else{state = 0; output.emit_whitespace_sequence(); return 1;}
				break;
			case 42:if(Is_hchar(c)){state = 42; result += Unicode2Utf8(c);}
				else if(c == '>'){state = 0; result += Unicode2Utf8(c); output.emit_header_name(result); result = ""; headerstart = false;}
				else{state = 0; throw logic_error("unterminated header name");return 1;}
				break;
			case 43:if(Is_qchar(c)){state = 43; result += Unicode2Utf8(c);}
				else if(c == '\"'){state = 0; result += Unicode2Utf8(c); output.emit_header_name(result); result = ""; headerstart = false;}
				else{state = 0; return 1;}
				break;
			case 44:if(c == '*'){state = 45; }
				else if(c == EndOfFile){cout<<"partial comment"<<endl;throw logic_error("partial comment");}
				break;
			case 45:if(c == '/'){
					if(blankfrom == 0)state=0;
					else if(blankfrom == 41)state = 41;
					else throw logic_error("invalid blankfrom");
					blankfrom = -1; 
					iinput[pos]=' ';
					result = "";
					return 1;
				}
				else if(c == '*')state=45;
				else if(c == EndOfFile){cout<<"partial comment"<<endl;throw logic_error("partial comment");}
				else state = 44;
				break;
			case 46:if(c == '='){state = 0; result += Unicode2Utf8(c); output.emit_preprocessing_op_or_punc(result); result = ""; headerstart = false;}
				else if(c == '/'){state = 37;result += Unicode2Utf8(c);}
				else if(c == '*'){state = 44;result += Unicode2Utf8(c);}
				else {state = 0; output.emit_preprocessing_op_or_punc(result); result = ""; headerstart = false; return 1;}
				break;

			case 47:if(Is_dchar(c)){state=47; 
					if(reversestring.find(pos) == reversestring.end()){
						result += Unicode2Utf8(c); 
						delimiter += char(c);
					}else{
						string rev = reversestring.at(pos);
						result += rev.substr(0,rev.size()-1); 
						delimiter += rev.substr(0,rev.size()-1);
						iinput[pos]=rev[rev.size()-1];
						reversestring.erase(pos);
						return 1;
					}
				}
				else if(c == '('){state=48; result += '(';}
				else {
					throw logic_error("error raw string!\n");
				}
				break;
			case 48:if(c == ')'){state=49; result += ')';}
				else if(c == -1){           //not a raw string			
					throw logic_error("not a raw string\n");
				}
				else{ state = 48;
					if(line_splice.find(pos) != line_splice.end()){//a line splicing before
						result += '\\';result += char(0x0a);line_splice.erase(pos);
					}
					if(reversestring.find(pos) == reversestring.end()){
						result += Unicode2Utf8(c); 
					}else{
						
						string rev = reversestring.at(pos);
						result += rev.substr(0,rev.size()-1); 
						iinput[pos]=rev[rev.size()-1];
						reversestring.erase(pos);
						return 1;
					}
				}
				break;
			case 49:if(line_splice.find(pos) != line_splice.end()){
					result += '\\';result += char(0x0a);
					line_splice.erase(pos);
					state = 48;
					return 1;
				}
				if(c == '\"'){
					result += Unicode2Utf8(c);
					state = 50;
				}else if(Is_dchar(c)){state=49;
					if(reversestring.find(pos) == reversestring.end()){
						result += Unicode2Utf8(c); 
						delimiter2 += char(c);
					}else{
						string rev = reversestring.at(pos);
						result += rev.substr(0,rev.size()-1); 
						delimiter2 += rev.substr(0,rev.size()-1);
						iinput[pos]=rev[rev.size()-1];
						reversestring.erase(pos);
						return 1;
					}
				}else if(c == ')'){
					result += ')';
					delimiter2 = "";
				}else{state = 48;result += Unicode2Utf8(c);}
				break;
			case 50:if(delimiter == delimiter2 && delimiter.length() <= 16){					
					delimiter = "";
					delimiter2 = "";
					if(Is_identifier_nondigit(c) || Digit.find(c) != Digit.end()){state = 15;result += Unicode2Utf8(c);}
					else {state = 0;output.emit_string_literal(result); result = ""; headerstart = false; return 1; }
				}else{
					//cout<<delimiter<<" "<<delimiter2<<endl;
					state = 48;
					delimiter2 = "";
					return 1;
				}
				break;
			case 51:if(c == '*' || c == '/'){state = 46;return 1;}
				else if(c == '='){state = 0;output.emit_whitespace_sequence();output.emit_preprocessing_op_or_punc("/=");}
				else {state = 0;output.emit_whitespace_sequence();output.emit_preprocessing_op_or_punc("/");return 1;}
				break;
			default:cout<<"Big DFA failed, what the fuck!"<<state<<endl;throw logic_error("Big DFA failed, what the fuck!");
				
		}		
		if (c == EndOfFile)
		{//cout<<"here  end"<<endl;
			output.emit_eof();
		}
		return 0;
		// TIP: Reference implementation is about 1000 lines of code.
		// It is a state machine with about 50 states, most of which
		// are simple transitions of the operators.
	}
};


///////////////////////////////////////////////////////////////////////////////////////////////////////////
//  The following are all PA4 code
//
///////////////////////////////////////////////////////////////////////////////////////////////////////////

class PA4_PreprocessingTokenizer:public IPPTokenStream{
public:
	
	static int state;

	const unordered_map<string, char> StringToType = {
		{"whitespace",0},
		{"newline",1},
		{"headname",2},
		{"identifier",3},
		{"ppnumber",4},
		{"character",5},
		{"udcharacter",6},
		{"string",7},
		{"udstring",8},
		{"poop",9},
		{"nonwhitespace",10},
		{"endmark",11},
		{"placemarker",12},
		{"eof", -1}
	};

	
	vector<PA4_pptoken*> textsequence;
	

	class Macro{
	public:
		string name;
		bool isfunc;
		//vector<string> parameters;
		map<string,int> parameter2pos;
		vector<PA4_pptoken*> replacementlist;
	
		Macro():name(""),isfunc(false){			
		}

		bool isfunction(){
			return isfunc;
		}

	};
	vector<Macro*> macros;
	map<string,int> Mapname2Mapindex;
	stack<int> undef_macro_pos_stack;
	Macro *Mmacro;

	void emit_whitespace_sequence(){
		string s;
		PA4_pptoken *pptoken = PA4_pptoken::GetWhiteSpace();
		PA4_DFA(pptoken);	
	}
	void emit_new_line(){
		string s;
		PA4_pptoken *pptoken = new PA4_pptoken(s,1);
		PA4_DFA(pptoken);	
	}
	void emit_header_name(const string& data){
		PA4_pptoken *pptoken = new PA4_pptoken(data,2);
		PA4_DFA(pptoken);
	}
	void emit_identifier(const string& data){
		PA4_pptoken *pptoken = new PA4_pptoken(data,3);
		PA4_DFA(pptoken);
	}
	void emit_pp_number(const string& data){
		PA4_pptoken *pptoken = new PA4_pptoken(data,4);
		PA4_DFA(pptoken);	
	}
	void emit_character_literal(const string& data){
		PA4_pptoken *pptoken = new PA4_pptoken(data,5);
		PA4_DFA(pptoken);	
	}
	void emit_user_defined_character_literal(const string& data){
		PA4_pptoken *pptoken = new PA4_pptoken(data,6);
		PA4_DFA(pptoken);	
	}
	void emit_string_literal(const string& data){
		PA4_pptoken *pptoken = new PA4_pptoken(data,7);
		PA4_DFA(pptoken);	
	}
	void emit_user_defined_string_literal(const string& data){
		PA4_pptoken *pptoken = new PA4_pptoken(data,8);
		PA4_DFA(pptoken);
	}
	void emit_preprocessing_op_or_punc(const string& data){
		PA4_pptoken *pptoken = new PA4_pptoken(data,9);
		PA4_DFA(pptoken);	
	}
	void emit_non_whitespace_char(const string& data){
		PA4_pptoken *pptoken = new PA4_pptoken(data,10);
		PA4_DFA(pptoken);	
	}
	void emit_eof(){
		PA4_DFA(PA4_pptoken::GetEOF());
		//output.emit_eof();	
	}

	bool MacroEqual(Macro *a,Macro *b){
		if(a -> name != b -> name){
			return false;
		}else if(a -> isfunction() != b -> isfunction()){
			return false;		
		}else if(a -> isfunction()){
			//cout<<"is function"<<endl;
			if(a -> parameter2pos.size() != b -> parameter2pos.size()){
				return false;
			}
			map<string,int>::iterator ita,itb;
			for(ita = a -> parameter2pos.begin(), itb = b -> parameter2pos.begin(); ita != a -> parameter2pos.end(),itb != b -> parameter2pos.end();ita++,itb++){
				if(ita -> first != itb -> first || ita -> second != itb -> second){
					return false;					
				}
			}
		}else{
			//cout<<"not function"<<endl;
			//printvector(a -> replacementlist);
			//printvector(b -> replacementlist);
			if(a -> replacementlist.size() != b -> replacementlist.size()){
				return false;
			}
			//cout<<"replacement list size are the same"<<endl;
			for(uint i = 0;i<a -> replacementlist.size();i++){
				if(a -> replacementlist[i] -> data != b -> replacementlist[i] -> data){
					return false;					
				}else if(a -> replacementlist[i] -> type != b -> replacementlist[i] -> type){
					return false;					
				}
			}
		}
		return true;
	}

	int GetMacroID(){
		if(undef_macro_pos_stack.empty()){
			return macros.size();	
		}else{
			int pos = undef_macro_pos_stack.top();	
			undef_macro_pos_stack.pop();
			return pos;			
		}
	}

	void PA4_DFA(PA4_pptoken *token){
		if(token -> type == StringToType.at("eof")){			
			replace_output(textsequence, 1);
			textsequence.clear();
			token -> out();
			return;
		}
		switch(state){
			case 0:if(token -> type == StringToType.at("whitespace") || token -> type == StringToType.at("newline")){state = 0;}
				else if(token -> data == "#") {state = 1;}
				else{
					state = 15;
					if(token -> data == "__VA_ARGS__"){
						throw logic_error("__VA_ARGS__ token in text-lines: __VA_ARGS__");
					}
					textsequence.push_back(token);
				}
				break;
			case 1:if(token -> type == StringToType.at("whitespace")){state = 1;}
				else if(token -> data == "define") {state = 2;}
				else if(token -> data == "undef") {state = 17;}
				else{throw logic_error("state 1 error");}
				break;
			case 2:if(token -> type == StringToType.at("whitespace")){state = 3;}
				else{throw logic_error("state 2 error");}
				break;
			case 3:if(token -> type == StringToType.at("whitespace")){state = 3;}
				else if(token -> type == StringToType.at("identifier")){
					state = 4;
					if(token -> data == "__VA_ARGS__"){
						throw logic_error("invalid __VA_ARGS__ use");
					}
					Mmacro = new Macro();
					Mmacro -> name = token -> data;
				}
				else{throw logic_error("state 3 error");}
				break;
			case 4:if(token -> type == StringToType.at("whitespace")){state = 13;Mmacro -> isfunc = false;}
				else if(token -> type == StringToType.at("newline")){
					state = 0;
					Mmacro -> isfunc = false;
					macros.push_back(Mmacro);
				}
				else if(token -> data == "("){
					state = 5;
					Mmacro -> isfunc = true;
				}
				else{throw logic_error("state 4 error");}
				break;
			case 5:if(token -> type == StringToType.at("whitespace")){state = 5;}
				else if(token -> type == StringToType.at("identifier")){
					state = 6;
					if(token -> data == "__VA_ARGS__"){
						throw logic_error("invalid __VA_ARGS__ use");
					}
					int pos = Mmacro -> parameter2pos.size();
					Mmacro -> parameter2pos[token -> data] = pos;
				}
				else if(token -> data == ")"){
					state = 11;
				}
				else if(token -> data == "..."){
					state = 10;
				}
				else{throw logic_error("state 5 error");}
				break;
			case 6:if(token -> type == StringToType.at("whitespace")){state = 6;}
				else if(token -> data == ")"){
					state = 11;
				}
				else if(token -> data == ","){
					state = 7;
				}
				else{throw logic_error("state 6 error");}
				break;
			case 7:if(token -> type == StringToType.at("whitespace")){state = 7;}
				else if(token -> type == StringToType.at("identifier")){
					state = 6;
					if(token -> data == "__VA_ARGS__"){
						throw logic_error("invalid __VA_ARGS__ use");
					}
					if(Mmacro -> parameter2pos.find(token -> data) != Mmacro -> parameter2pos.end()){
						throw logic_error("duplicate parameter " + token -> data + " in macro definition");
					}else{
						int pos = Mmacro -> parameter2pos.size();
						Mmacro -> parameter2pos[token -> data] = pos;
					}
				}
				else if(token -> data == "..."){
					state = 10;
				}
				else{throw logic_error("state 7 error");}
				break;	
			case 8:if(token -> type == StringToType.at("whitespace")){state = 8;}
				else if(token -> data != "__VA_ARGS__" && Mmacro -> parameter2pos.find(token -> data) == Mmacro -> parameter2pos.end()){
					throw logic_error("# must be followed by parameter in function-like macro");					
				}else{
					state = 12;
					Mmacro -> replacementlist.push_back(token);
				}
				break;		
			case 9:if(token -> type == StringToType.at("whitespace")){state = 9;}
				else if(token -> type == StringToType.at("newline")){
					throw logic_error("## at edge of replacement list");				
				}else{
					state = 12;
					Mmacro -> replacementlist.push_back(token);
				}
				break;		
			case 10:if(token -> type == StringToType.at("whitespace")){state = 10;}
				else if(token -> data == ")"){
					int pos = Mmacro -> parameter2pos.size();
					Mmacro -> parameter2pos["..."] = pos;
					state = 11;
				}
				else{throw logic_error("state 10 error");}
				break;
			case 11:if(token -> type == StringToType.at("whitespace")){state = 11;}
				else if(token -> data == "#"){
					state = 8;
					Mmacro -> replacementlist.push_back(token);	
				}else if(token -> data == "##"){
					throw logic_error("## at edge of replacement list");	
				}else if(token -> type == StringToType.at("newline")){
					state = 0;
					while(Mmacro -> replacementlist.back() -> type == StringToType.at("whitespace")){
						Mmacro -> replacementlist.pop_back();
					}
					Mmacro -> replacementlist.push_back(PA4_pptoken::GetEndMark());
					if(Mapname2Mapindex.find(Mmacro -> name)!= Mapname2Mapindex.end()){
						if(MacroEqual(Mmacro,macros[Mapname2Mapindex[Mmacro -> name]])){
							//throw logic_error("ERROR: macro redefined\n");
						}else{
							throw logic_error(" macro with the same name\n");
						}
					}else{
						Mapname2Mapindex[Mmacro -> name] = GetMacroID();
						if(Mapname2Mapindex[Mmacro -> name] == (int)macros.size()){
							macros.push_back(Mmacro);
						}else{
							macros[Mapname2Mapindex[Mmacro -> name] ] = Mmacro;
						}
					}
				}else{
					state = 12;
					if(Mmacro -> parameter2pos.find("...") == Mmacro -> parameter2pos.end() && token -> data == "__VA_ARGS__"){
						throw logic_error(" invalid __VA_ARGS__ use");
					}
					Mmacro -> replacementlist.push_back(token);					
				}
				break;
			case 12:if(token -> type == StringToType.at("newline")){
					state = 0;
					while(Mmacro -> replacementlist.back() -> type == StringToType.at("whitespace")){
						Mmacro -> replacementlist.pop_back();
					}
					Mmacro -> replacementlist.push_back(PA4_pptoken::GetEndMark());
					if(Mapname2Mapindex.find(Mmacro -> name)!= Mapname2Mapindex.end()){
						if(MacroEqual(Mmacro,macros[Mapname2Mapindex[Mmacro -> name]])){
							//throw logic_error("ERROR: macro redefined\n");
						}else{
							throw logic_error(" macro with the same name\n");
						}
					}else{
						Mapname2Mapindex[Mmacro -> name] = GetMacroID();
						if(Mapname2Mapindex[Mmacro -> name] == (int)macros.size()){
							macros.push_back(Mmacro);
						}else{
							macros[Mapname2Mapindex[Mmacro -> name] ] = Mmacro;
						}
					}
				}else if(token -> data == "#"){
					state = 8;
					Mmacro -> replacementlist.push_back(token);	
				}else if(token -> data == "##"){
					state = 9;
					Mmacro -> replacementlist.push_back(token);	
				}else{
					state = 12;
					if(Mmacro -> parameter2pos.find("...") == Mmacro -> parameter2pos.end() && token -> data == "__VA_ARGS__"){
						throw logic_error(" invalid __VA_ARGS__ use");
					}
					Mmacro -> replacementlist.push_back(token);					
				}
				break;
			case 13:if(token -> type == StringToType.at("whitespace")){state = 13;}
				else if(token -> type == StringToType.at("newline")){
					state = 0;
					while(Mmacro -> replacementlist.back() -> type == StringToType.at("whitespace")){
						Mmacro -> replacementlist.pop_back();
					}
					Mmacro -> replacementlist.push_back(PA4_pptoken::GetEndMark());
					if(Mapname2Mapindex.find(Mmacro -> name)!= Mapname2Mapindex.end()){
						if(MacroEqual(Mmacro,macros[Mapname2Mapindex[Mmacro -> name]])){
							//throw logic_error("ERROR: macro redefined\n");
						}else{
							throw logic_error(" macro with the same name\n");
						}
					}else{
						Mapname2Mapindex[Mmacro -> name] = GetMacroID();
						if(Mapname2Mapindex[Mmacro -> name] == (int) macros.size()){
							macros.push_back(Mmacro);
						}else{
							macros[Mapname2Mapindex[Mmacro -> name] ] = Mmacro;
						}
					}
				}else if(token -> data == "##"){
					throw logic_error("## at edge of replacement list");	
				}else{
					state = 14;
					if(token -> data == "__VA_ARGS__"){
						throw logic_error("invalid __VA_ARGS__ use");
					}
					Mmacro -> replacementlist.push_back(token);					
				}
				break;
			case 14:if(token -> type == StringToType.at("newline")){
					state = 0;
					while(Mmacro -> replacementlist.back() -> type == StringToType.at("whitespace")){
						Mmacro -> replacementlist.pop_back();
					}
					Mmacro -> replacementlist.push_back(PA4_pptoken::GetEndMark());
					if(Mapname2Mapindex.find(Mmacro -> name)!= Mapname2Mapindex.end()){
						if(MacroEqual(Mmacro,macros[Mapname2Mapindex[Mmacro -> name]])){
							//throw logic_error("ERROR: macro redefined\n");
						}else{
							throw logic_error(" macro with the same name\n");
						}
					}else{
						
						Mapname2Mapindex[Mmacro -> name] = GetMacroID();
						if(Mapname2Mapindex[Mmacro -> name] == (int) macros.size()){
							macros.push_back(Mmacro);
						}else{
							macros[Mapname2Mapindex[Mmacro -> name] ] = Mmacro;
						}
					}
				}else if(token -> data == "##"){
					state = 20;
					Mmacro -> replacementlist.push_back(token);
				}else{
					state = 14;
					if(token -> data == "__VA_ARGS__"){
						throw logic_error("invalid __VA_ARGS__ use");
					}
					Mmacro -> replacementlist.push_back(token);					
				}
				break;
			case 15:if(token -> type == StringToType.at("newline")){
					state = 16;                            //notice
					textsequence.push_back(PA4_pptoken::GetWhiteSpace());
				}else{
					state = 15;
					if(token -> data == "__VA_ARGS__"){
						throw logic_error("__VA_ARGS__ token in text-lines: __VA_ARGS__");
					}
					textsequence.push_back(token);
				}//cout<<"state 15"<<token -> data<<" "<<token -> type<<endl;
				break;
			case 16:if(token -> data == "#"){
					state = 1;		
					replace_output(textsequence, 1);
					textsequence.clear();
				}else if(token -> type == StringToType.at("whitespace") || token -> type == StringToType.at("newline")){
					state = 16;
				}else{
					state = 15;
					if(token -> data == "__VA_ARGS__"){
						throw logic_error("__VA_ARGS__ token in text-lines: __VA_ARGS__");
					}
					textsequence.push_back(token);
				}//cout<<"state 16"<<token -> data<<" "<<token -> type<<endl;
				break;
			case 17:if(token -> type == StringToType.at("whitespace")){state = 18;}
				else{throw logic_error("state 17 error");}
				break;
			case 18:if(token -> type == StringToType.at("whitespace")){state = 18;}
				else if(token -> type == StringToType.at("identifier")){
					state = 19;
					if(token -> data == "__VA_ARGS__"){
						throw logic_error("invalid __VA_ARGS__ use");
					}
					for(uint iundef = 0;iundef < macros.size();iundef++){
						if(macros[iundef] -> name == token -> data){
							undef_macro_pos_stack.push(iundef);    // add the undef macro pos to the stack
							Mapname2Mapindex.erase(token -> data);
							break;
						}
					}
				}
				else{throw logic_error("state 18 error");}
				break;
			case 19:if(token -> type == StringToType.at("whitespace")){state = 19;}
				else if(token -> type == StringToType.at("newline")){
					state = 0;
				}
				else{throw logic_error("state 19 error");}
				break;
			case 20:if(token -> type == StringToType.at("whitespace")){state = 20;}
				else if(token -> type == StringToType.at("newline")){
					throw logic_error("## at edge of replacement list");				
				}else{
					state = 14;
					Mmacro -> replacementlist.push_back(token);
				}
				break;
		}
	}

	vector<vector<PA4_pptoken*>> GetRealParameters(stack<PA4_pptoken*>& ts, Macro* rid){
		int VApos = -1;
		vector<vector<PA4_pptoken*>> res;
		vector<PA4_pptoken*> *p = new vector<PA4_pptoken*>();
		if(rid -> parameter2pos.find("...") != rid -> parameter2pos.end()){
			VApos = rid -> parameter2pos.at("...");
		}
		int left = 0,right = 0;
		while(!(left == right && left != 0)){
			PA4_pptoken *curr = ts.top();
			ts.pop();
			if(curr -> data == "("){
				if(left > 0){           
					p -> push_back(curr);	
				}
				left++;
			}else if(curr -> data == ")"){
				right++;
				if(left == right && left != 0){      
					res.push_back(*p);					
				}else{
					p -> push_back(curr);
				}
			}else if(curr -> data == ","){
				if(left == right + 1){
					if(VApos < 0){                                    // Don't have "..."
						if(!p -> empty() && p -> back() -> type == StringToType.at("whitespace")){
							p -> pop_back();
						}
						res.push_back(*p);
						p = new vector<PA4_pptoken*>();
					}else if(res.size() < VApos){                     //  have "..."
						if(!p -> empty() && p -> back() -> type == StringToType.at("whitespace")){
							p -> pop_back();
						}
						res.push_back(*p);
						p = new vector<PA4_pptoken*>();
					}else{
						p -> push_back(curr);
					}
				}else{    // if the ',' is in inner function
					p -> push_back(curr);
				}
			}else{
				if(p -> empty() && curr -> type == StringToType.at("whitespace")){}
				else{p -> push_back(curr);}
			}			
		}
		//for(int i = 0;i<res.size();i++){
		//	for(int j=0;j<res[i].size();j++){
		//		cout<<res[i][j]->data<<'('<<res[i][j]->type<<')'<<' ';
		//	}cout<<endl;
		//}
		return res;
	}

	vector<PA4_pptoken*> ParameterReplacer(vector<vector<PA4_pptoken*>> real_parameters, Macro* rid, PA4_pptoken* source){
		//cout<<"ParameterReplacer "<<rid -> name<<endl;
		int  state = 0;
		string cat="";
		vector<PA4_pptoken*> sequence; // the sequence after replacement
//printvector(rid -> replacementlist);
		for(uint i = 0; i < rid -> replacementlist.size(); i++){
			rid -> replacementlist[i] -> InheritBlackList(source);
			rid -> replacementlist[i] -> blacklist.insert(rid -> name);
			PA4_pptoken *ptext = rid -> replacementlist[i];
//cout<<cat<<" 0 "<<ptext -> data<<endl;
			switch(state){
				case 0:if(ptext -> data == "#"){state = 1;}
					else if(ptext -> data == "##"){
						state = 2;
						while(sequence.back() -> type == StringToType.at("whitespace"))
							sequence.pop_back();     // kick extra white space

						if(i == 0 || i == rid -> replacementlist.size() - 1){
							throw logic_error(" ## at edge of replacement list");
						}else{
							cat += sequence[sequence.size()-1] -> data; // catenate the last token
							sequence.pop_back();
						}
					}
					else if(ptext -> type == StringToType.at("identifier")){
						state = 0;
						if(rid -> parameter2pos.find(ptext -> data) != rid -> parameter2pos.end()){
							 // The __VA_ARGS__ in parameters should be kicked out in the automata!!!
							vector<PA4_pptoken*> parameter = real_parameters[rid -> parameter2pos.at(ptext -> data)];

							int next_sense_pos = 1;
							while(i + next_sense_pos < rid -> replacementlist.size() && rid -> replacementlist[i + next_sense_pos] -> type == StringToType.at("whitespace")){
								next_sense_pos ++;
							}

							if(rid -> replacementlist[i + next_sense_pos] -> data == "##"){
								for(uint j = 0 ; j < parameter.size();j++){								
									parameter[j] -> InheritBlackList(source);
									parameter[j] -> blacklist.insert(rid -> name);
									sequence.push_back(PA4_pptoken::GetNew(parameter[j]));
								}
								if(parameter.size() == 0){
									sequence.push_back(PA4_pptoken::GetPlaceMarker());
								}
							}else{//cout<<"here1"<<ptext->data<<endl;
								vector<PA4_pptoken*> res = replace_output(parameter, 0);
								//cout<<"here2"<<ptext->data<<endl;
								for(uint j = 0 ; j < res.size();j++){								
									res[j] -> InheritBlackList(source);
									res[j] -> blacklist.insert(rid -> name);
									sequence.push_back(PA4_pptoken::GetNew(res[j]));
								}
							}

							
						}else if(ptext -> data == "__VA_ARGS__"){
							if(rid -> parameter2pos.find("...") == rid -> parameter2pos.end()){
								throw logic_error(" invalid __VA_ARGS__ use");
							}else{
								int VApos = rid -> parameter2pos.at("...");
								vector<PA4_pptoken*> parameter = real_parameters[VApos];
								for(uint j = 0; j < parameter.size();j++){									
									parameter[j] -> InheritBlackList(source);
									parameter[j] -> blacklist.insert(rid -> name);
									sequence.push_back(PA4_pptoken::GetNew(parameter[j]));
								}
							}
						}else{sequence.push_back(PA4_pptoken::GetNew(ptext));}
					}else if(ptext -> type == StringToType.at("whitespace")){
						state = 0;
						sequence.push_back(PA4_pptoken::GetWhiteSpace());
					}else{
						if(ptext -> type != StringToType.at("endmark"))
							sequence.push_back(PA4_pptoken::GetNew(ptext));
						state = 0;
					}
					break;
				case 1:if(ptext -> data == "##"){
						state = 2;
						if(i == 0 || i == rid -> replacementlist.size() - 1){
							throw logic_error(" ## at edge of replacement list");
						}else{
							cat += "#"; // catenate "#"
						}
					}else if(ptext -> type == StringToType.at("whitespace")){
						state = 1;
					}else if(rid -> parameter2pos.find(ptext -> data) != rid -> parameter2pos.end()){//the argument is stringized
						state = 0;
						int strpos = rid -> parameter2pos.at(ptext -> data);
						vector<PA4_pptoken*> parameter = real_parameters[strpos];
						string macrostr;
						//printvector(parameter);
						for(uint j = 0; j < parameter.size(); j++){
							if(parameter[j] -> type == StringToType.at("character") || 
								parameter[j] -> type == StringToType.at("udcharacter") || 
								parameter[j] -> type == StringToType.at("string") || 								
								parameter[j] -> type == StringToType.at("udstring")){ 

								for(uint k = 0; k < parameter[j] -> data.length(); k++){
									if(parameter[j] -> data[k] == '\"' || parameter[j] -> data[k] == '\\'){
										macrostr += "\\";
										macrostr += parameter[j] -> data[k];
									}else if(parameter[j] -> data[k] == ' ' ){
										if(macrostr[macrostr.length()-1] != ' ')
											macrostr += ' ';
									}else{
										macrostr += parameter[j] -> data[k];
									}
								}
							}else if(parameter[j] -> type == StringToType.at("whitespace")){
								if(macrostr[macrostr.length()-1] != ' ')
									macrostr += ' ';
							}else{
								macrostr += parameter[j] -> data;
							}
						}
						int spacenum = 0;
						while(macrostr[macrostr.length()-spacenum-1] == ' ') spacenum++;
						
						PA4_pptoken* str = new PA4_pptoken("\"" + macrostr.substr(0,macrostr.length()-spacenum) + "\"", StringToType.at("string"));
						sequence.push_back(str);
					}else if(ptext -> data == "__VA_ARGS__"){
						state = 0;
						if(rid -> parameter2pos.find("...") == rid -> parameter2pos.end()){
							throw logic_error(" invalid __VA_ARGS__ use");
						}else{
							int VApos = rid -> parameter2pos.at("...");
							vector<PA4_pptoken*> parameter = real_parameters[VApos];
							string macrostr;
							//printvector(parameter);
							for(uint j = 0; j < parameter.size(); j++){
								if(parameter[j] -> type == StringToType.at("character") || 
									parameter[j] -> type == StringToType.at("udcharacter") || 
									parameter[j] -> type == StringToType.at("string") || 								
									parameter[j] -> type == StringToType.at("udstring")){ 

									for(uint k = 0; k < parameter[j] -> data.length(); k++){
										if(parameter[j] -> data[k] == '\"' || parameter[j] -> data[k] == '\\'){
											macrostr += "\\";
											macrostr += parameter[j] -> data[k];
										}else if(parameter[j] -> data[k] == ' ' ){
											if(macrostr[macrostr.length()-1] != ' ')
												macrostr += ' ';
										}else{
											macrostr += parameter[j] -> data[k];
										}
									}
								}else if(parameter[j] -> type == StringToType.at("whitespace")){
									if(macrostr[macrostr.length()-1] != ' ')
										macrostr += ' ';
								}else{
									macrostr += parameter[j] -> data;
								}
							}
							int spacenum = 0;
							while(macrostr[macrostr.length()-spacenum-1] == ' ') spacenum++;
						
							PA4_pptoken* str = new PA4_pptoken("\"" + macrostr.substr(0,macrostr.length()-spacenum) + "\"", StringToType.at("string"));
							sequence.push_back(str);
						}
					}else{
						throw logic_error(" # must be followed by parameter in function-like macro(#1)");
					}
					break;
				case 2:state = 3;	
					if(ptext -> data == "#"){
						int next_sense_pos = 1;
						while(i + next_sense_pos < rid -> replacementlist.size() && rid -> replacementlist[i + next_sense_pos] -> type == StringToType.at("whitespace")){
							next_sense_pos ++;
						}
						string AfterSharp = rid -> replacementlist[i + next_sense_pos] -> data;
						if(rid -> parameter2pos.find(AfterSharp) != rid -> parameter2pos.end()){
							state = 4;
						}else{
							state = 3;
							cat += ptext -> data;
						}
					}else if(rid -> parameter2pos.find(ptext -> data) != rid -> parameter2pos.end()){
						int strpos = rid -> parameter2pos.at(ptext -> data);
						vector<PA4_pptoken*> parameter = real_parameters[strpos];
						for(uint j = 0;j < parameter.size(); j++){
							cat += parameter[j] -> data;
							if(j != parameter.size() - 1){
								cat += ' ';
							}
						}
						//	cout<<cat<<" 2"<<endl;						
					}else if(ptext -> type == StringToType.at("whitespace")){
						state = 2;
					}else{
						cat += ptext -> data;
						//	cout<<cat<<" 3 "<<ptext -> data<<' '<<ptext -> type<<endl;
					}
					break;
				case 3:if(ptext -> data == "#"){state = 4;}
					else if(ptext -> data == "##"){state = 2;}
					else if(ptext -> type == StringToType.at("whitespace")){
						state = 3;
					}else{
						if(ptext -> type != StringToType.at("endmark")) i--;
						state = 0;
		//cout<<cat<<endl;

						cat += char(-1);
		
						vector<int> catcode = PPTranslator::Translate(cat);		
	
						catcode.push_back(-1);

						PA4_IPPTokenStreamRecorder recorder;

						PA1_PPTokenizer retokenizer(recorder);

						for(uint j=0;j < catcode.size();j++)
						{	
							int code_unit = catcode[j];
							if(retokenizer.process(code_unit,j)){
								j=j-1;
							} 
						}
						vector<PA4_pptoken*> cattoken = recorder.GetRecord();  //printvector(cattoken);
						for(uint j = 0; j < cattoken.size(); j++){
							cattoken[j] -> InheritBlackList(source);
							cattoken[j] -> blacklist.insert(rid -> name);
							sequence.push_back(cattoken[j]);
						}

						cat = "";										
					}
					break;
				case 4:if(rid -> parameter2pos.find(ptext -> data) != rid -> parameter2pos.end()){//nearly the same as state 1
						state = 3;
						int strpos = rid -> parameter2pos.at(ptext -> data);
						vector<PA4_pptoken*> parameter = real_parameters[strpos];
						string macrostr;
						for(uint j = 0; j < parameter.size(); j++){
							if(parameter[j] -> type == StringToType.at("character") || 
								parameter[j] -> type == StringToType.at("udcharacter") || 
								parameter[j] -> type == StringToType.at("string") || 								
								parameter[j] -> type == StringToType.at("udstring")){ 

								for(uint k = 0; k < parameter[j] -> data.length(); k++){
									if(parameter[j] -> data[k] == '\"' || parameter[j] -> data[k] == '\\'){
										macrostr += "\\";
										macrostr += parameter[j] -> data[k];
									}else if(parameter[j] -> data[k] == ' ' ){
										if(macrostr[macrostr.length()-1] != ' ')
											macrostr += ' ';
									}else{
										macrostr += parameter[j] -> data[k];
									}
								}
							}else if(parameter[j] -> type == StringToType.at("whitespace")){
								if(macrostr[macrostr.length()-1] != ' ')
									macrostr += ' ';
							}else{
								macrostr += parameter[j] -> data;
							}
						}
						int spacenum = 0;
						while(macrostr[macrostr.length()-spacenum-1] == ' ') spacenum++;
						
						string str = "\"" + macrostr.substr(0,macrostr.length()-spacenum) + "\"";
						
						cat += str;                   // different from state 1
					///		cout<<cat<<" 5"<<endl;
					}else{
						throw logic_error(" # must be followed by parameter in function-like macro(#2)");
					}
					break;
			}	
		}
//cout<<"ParameterReplacer end "<<rid -> name<<endl;
		return sequence;
		
	}

	void printvector(vector<PA4_pptoken*> v){
		cout<<"vector size: "<<v.size()<<endl;
		for(uint i = 0;i<v.size();i++){
			cout<<v[i]->data<<'('<<v[i]->type<<')'<<' ';
		}cout<<endl<<"vector end"<<endl;
	}

	void printstack(stack<PA4_pptoken*> sta){
		cout<<"stack size: "<<sta.size()<<endl;
		stack<PA4_pptoken*> s(sta);
		while(!s.empty()){ cout<<s.top()->data<<'('<<s.top()->type<<')'<<' ';s.pop();
		}
		cout<<endl<<"stack end"<<endl;
	}
		
	void printblacklist(PA4_pptoken* curr){
		cout<<"blacklist size: "<<curr->blacklist.size()<<endl;
		copy(curr->blacklist.begin(), curr->blacklist.end(), ostream_iterator<string>(cout, ", ") );
		cout<<endl<<"blacklist end"<<endl;
		
	}

	void printmacro(){
		for(uint i=0;i<macros.size();i++){
		
			cout<<macros[i]->name<<" "<<macros[i]->isfunc<<" "<<endl;
			printvector(macros[i]->replacementlist);		
				
		}
	}

	void MacroReplace(PA4_pptoken* curr, stack<PA4_pptoken*>& ts, bool needoutput, vector<PA4_pptoken*>& recordsequence){
		
		//cout<<"MacroReplace "<<curr->data<<endl;
		Macro* rid = macros[Mapname2Mapindex.at(curr -> data)];
		if(rid -> isfunction()){ 
			while(!ts.empty() && ts.top() -> type == StringToType.at("whitespace"))
				ts.pop();
			if(ts.empty() || ts.top() -> data != "("){  // although it has the same name with a func macro, but it is not used as a macro
				if(needoutput){
					curr -> out();
					delete curr;
				}else{	
					recordsequence.push_back(PA4_pptoken::GetNew(curr));
				} 
			}else{
				vector<vector<PA4_pptoken*>> real_parameters = GetRealParameters(ts,rid);
				vector<PA4_pptoken*> postreplace = ParameterReplacer(real_parameters, rid, curr);//cout<<"222"<<endl;printvector(postreplace);
				
				for(int i = postreplace.size()-1;i>=0;i--){
					ts.push(postreplace[i]);
				}			
			}
		}else{
			vector<vector<PA4_pptoken*>> real_parameters;
			vector<PA4_pptoken*> postreplace = ParameterReplacer(real_parameters, rid, curr);
			
			for(int i = postreplace.size()-1;i>=0;i--){
				ts.push(postreplace[i]);
			}
		}
		//cout<<"MacroReplace end "<<curr->data<<endl;
	}

	vector<PA4_pptoken*> replace_output(vector<PA4_pptoken*> sequence, bool needoutput){
		//printmacro();
		vector<PA4_pptoken*>  result;
		stack<PA4_pptoken*> ts;
		for(int i = sequence.size()-1; i >= 0; i--){
			ts.push(sequence[i]);
		}
		while(!ts.empty()){
			//if(needoutput)
			//	printstack(ts);
			PA4_pptoken *curr = ts.top();	
					//	cout<<"black1"<<endl;printblacklist(curr);
			ts.pop();
			if(curr -> type == StringToType.at("identifier")){
				if(Mapname2Mapindex.find(curr -> data) != Mapname2Mapindex.end()){
					if(curr -> blacklist.find(curr -> data) == curr -> blacklist.end()){ // it is not in the blacklist
						if(needoutput){
							vector<PA4_pptoken*> empty;
							MacroReplace(curr, ts, 1, empty);
						}else{
							MacroReplace(curr, ts, 0, result);
							
						}
					}else{
						if(needoutput){
							curr -> out();
							delete curr;
						}else{
							result.push_back(curr);
						}
					}
				}else{
					if(needoutput){
						curr -> out();
						delete curr;
					}else{
						result.push_back(curr);
					}
				}
			}else{ 
				if(needoutput){
					curr -> out();
					delete curr;
				}else{
					result.push_back(curr);
				}
			}
		}
		
		//cout<<"replace_output end"<<endl;
		return result;
	}

	virtual ~PA4_PreprocessingTokenizer() {}

};
int PA4_PreprocessingTokenizer::state = 0;
///////////////////////////////////////////////////////////////////////////////////////////////////////////
//  The above are all PA4 code
//
///////////////////////////////////////////////////////////////////////////////////////////////////////////


int main()
{
	try
	{
		ostringstream oss;
		oss << cin.rdbuf();

		string input = oss.str();
	
		input += char(-1);
		
		iinput = PPTranslator::Translate(input);		
	
		iinput.push_back(-1);

		PA4_PreprocessingTokenizer output;

		PA1_PPTokenizer tokenizer(output);

		for(uint i=0;i<iinput.size();i++)
		{	
			int code_unit = iinput[i];
			if(tokenizer.process(code_unit,i)){
				i=i-1;
			}
		}

	}
	catch (exception& e)
	{
		cerr << "ERROR: " << e.what() << endl;
		return EXIT_FAILURE;
	}
}
