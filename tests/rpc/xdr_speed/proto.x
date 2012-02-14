enum abc {
    A = 0,
    B = 1,
    C = 2
};


union u_abc switch (abc c) {
 case A:
     struct {
	 string a1<>;
	 string a2<>;
     } a;
 case B:
     int b;
 case C:
     void;
};


typedef string string_20<20>;


struct str {
    int i;
    string s<>;
    int ia[20];
    string_20 sa[20];
    u_abc *abc;
};


typedef str arr<>;


program P {
    version V {
	str do_something(str) = 1;
    } = 1;
} = 200000;
