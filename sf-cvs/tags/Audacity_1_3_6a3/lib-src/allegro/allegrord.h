#include "../../src/Experimental.h"

#define field_max 80

#ifndef EXPERIMENTAL_NOTE_TRACK
class Allegro_reader {
public:
    FILE *file;
    int line_no;
    String_parse line_parser;
    bool line_parser_flag;
    char field[field_max];
    bool error_flag;
    Seq seq;
    double tsnum;
    double tsden;
#else /* EXPERIMENTAL_NOTE_TRACK */
class Alg_reader {
public:
    FILE *file;
    int line_no;
    String_parse line_parser;
    bool line_parser_flag;
    char field[field_max];
    bool error_flag;
    Alg_seq_ptr seq;
    double tsnum;
    double tsden;
#endif /* EXPERIMENTAL_NOTE_TRACK */

#ifndef EXPERIMENTAL_NOTE_TRACK
    Allegro_reader(FILE *a_file);
    void readline();
    void process_attributes(Parameters_ptr attributes, double time);
    bool parse();
    long parse_int(char *field);
    int find_real_in(char *field, int n);
    double parse_real(char *field);
    void parse_error(char *field, long offset, char *message);
    double parse_dur(char *field, double base);
    double parse_after_dur(double dur, char *field, int n, double base);
    double parse_loud(char *field);
    long parse_key(char *field);
    double parse_pitch(char *field);
    long parse_after_key(int key, char *field, int n);
    long find_int_in(char *field, int n);
    bool parse_attribute(char *field, Parameter_ptr parm);
    bool parse_val(Parameter_ptr param, char *s, int i);
    bool check_type(char type_char, Parameter_ptr param);
};
#else /* EXPERIMENTAL_NOTE_TRACK */
    Alg_reader(FILE *a_file, Alg_seq_ptr new_seq);
    void readline();
    Alg_parameters_ptr process_attributes(Alg_parameters_ptr attributes, double time);
    bool parse();
	long parse_chan(char *field);
    long parse_int(char *field);
    int find_real_in(char *field, int n);
    double parse_real(char *field);
    void parse_error(char *field, long offset, char *message);
    double parse_dur(char *field, double base);
    double parse_after_dur(double dur, char *field, int n, double base);
    double parse_loud(char *field);
    long parse_key(char *field);
    double parse_pitch(char *field);
    long parse_after_key(int key, char *field, int n);
    long find_int_in(char *field, int n);
    bool parse_attribute(char *field, Alg_parameter_ptr parm);
    bool parse_val(Alg_parameter_ptr param, char *s, int i);
    bool check_type(char type_char, Alg_parameter_ptr param);
};
#endif /* EXPERIMENTAL_NOTE_TRACK */
