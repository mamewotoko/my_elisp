/*
 * roma kana converter rule structure
 *
 * $Id: rkmap.h,v 1.6 2002/11/05 15:38:58 yusuke Exp $
 */

static const struct rk_rule rk_rule_alphabet[] = 
{
	{"a", "a", NULL},
	{"b", "b", NULL},
	{"c", "c", NULL},
	{"d", "d", NULL},
	{"e", "e", NULL},
	{"f", "f", NULL},
	{"g", "g", NULL},
	{"h", "h", NULL},
	{"i", "i", NULL},
	{"j", "j", NULL},
	{"k", "k", NULL},
	{"l", "l", NULL},
	{"m", "m", NULL},
	{"n", "n", NULL},
	{"o", "o", NULL},
	{"p", "p", NULL},
	{"q", "q", NULL},
	{"r", "r", NULL},
	{"s", "s", NULL},
	{"t", "t", NULL},
	{"u", "u", NULL},
	{"v", "v", NULL},
	{"w", "w", NULL},
	{"x", "x", NULL},
	{"y", "y", NULL},
	{"z", "z", NULL},
	{"A", "A", NULL},
	{"B", "B", NULL},
	{"C", "C", NULL},
	{"D", "D", NULL},
	{"E", "E", NULL},
	{"F", "F", NULL},
	{"G", "G", NULL},
	{"H", "H", NULL},
	{"I", "I", NULL},
	{"J", "J", NULL},
	{"K", "K", NULL},
	{"L", "L", NULL},
	{"M", "M", NULL},
	{"N", "N", NULL},
	{"O", "O", NULL},
	{"P", "P", NULL},
	{"Q", "Q", NULL},
	{"R", "R", NULL},
	{"S", "S", NULL},
	{"T", "T", NULL},
	{"U", "U", NULL},
	{"V", "V", NULL},
	{"W", "W", NULL},
	{"X", "X", NULL},
	{"Y", "Y", NULL},
	{"Z", "Z", NULL},

	{NULL, NULL, NULL}
};

static const struct rk_rule rk_rule_walphabet[] = 
{
	{"a", "��", NULL},
	{"b", "��", NULL},
	{"c", "��", NULL},
	{"d", "��", NULL},
	{"e", "��", NULL},
	{"f", "��", NULL},
	{"g", "��", NULL},
	{"h", "��", NULL},
	{"i", "��", NULL},
	{"j", "��", NULL},
	{"k", "��", NULL},
	{"l", "��", NULL},
	{"m", "��", NULL},
	{"n", "��", NULL},
	{"o", "��", NULL},
	{"p", "��", NULL},
	{"q", "��", NULL},
	{"r", "��", NULL},
	{"s", "��", NULL},
	{"t", "��", NULL},
	{"u", "��", NULL},
	{"v", "��", NULL},
	{"w", "��", NULL},
	{"x", "��", NULL},
	{"y", "��", NULL},
	{"z", "��", NULL},
	{"A", "��", NULL},
	{"B", "��", NULL},
	{"C", "��", NULL},
	{"D", "��", NULL},
	{"E", "��", NULL},
	{"F", "��", NULL},
	{"G", "��", NULL},
	{"H", "��", NULL},
	{"I", "��", NULL},
	{"J", "��", NULL},
	{"K", "��", NULL},
	{"L", "��", NULL},
	{"M", "��", NULL},
	{"N", "��", NULL},
	{"O", "��", NULL},
	{"P", "��", NULL},
	{"Q", "��", NULL},
	{"R", "��", NULL},
	{"S", "��", NULL},
	{"T", "��", NULL},
	{"U", "��", NULL},
	{"V", "��", NULL},
	{"W", "��", NULL},
	{"X", "��", NULL},
	{"Y", "��", NULL},
	{"Z", "��", NULL},

	{NULL, NULL, NULL}
};

#define SKK_LIKE_KIGO_MAP \
	{"z/", "��", NULL}, \
	{"z[", "��", NULL}, \
	{"z]", "��", NULL}, \
	{"z,", "��", NULL}, \
	{"z.", "��", NULL}, \
	{"z-", "��", NULL}, \
	{"zh", "��", NULL}, \
	{"zj", "��", NULL}, \
	{"zk", "��", NULL}, \
	{"zl", "��", NULL}

static const struct rk_rule rk_rule_hiragana[] =
{
	SKK_LIKE_KIGO_MAP,

	{"a", "��", NULL},
	{"i", "��", NULL},
	{"u", "��", NULL},
	{"e", "��", NULL},
	{"o", "��", NULL},

	{"xa", "��", NULL},
	{"xi", "��", NULL},
	{"xu", "��", NULL},
	{"xe", "��", NULL},
	{"xo", "��", NULL},
	
	{"ka", "��", NULL},
	{"ki", "��", NULL},
	{"ku", "��", NULL},
	{"ke", "��", NULL},
	{"ko", "��", NULL},

	{"kya", "����", NULL},
	{"kyi", "����", NULL},
	{"kyu", "����", NULL},
	{"kye", "����", NULL},
	{"kyo", "����", NULL},
    
	{"k", "��", "k"},

	{"ga", "��", NULL},
	{"gi", "��", NULL},
	{"gu", "��", NULL},
	{"ge", "��", NULL},
	{"go", "��", NULL},

	{"gya", "����", NULL},
	{"gyi", "����", NULL},
	{"gyu", "����", NULL},
	{"gye", "����", NULL},
	{"gyo", "����", NULL},
    
	{"g", "��", "g"},

	{"sa", "��", NULL},
	{"si", "��", NULL},
	{"su", "��", NULL},
	{"se", "��", NULL},
	{"so", "��", NULL},

	{"sya", "����", NULL},
	{"syi", "����", NULL},
	{"syu", "����", NULL},
	{"sye", "����", NULL},
	{"syo", "����", NULL},
    
	{"sha", "����", NULL},
	{"shi", "��", NULL},
	{"shu", "����", NULL},
	{"she", "����", NULL},
	{"sho", "����", NULL},

	{"s", "��", "s"},

	{"za", "��", NULL},
	{"zi", "��", NULL},
	{"zu", "��", NULL},
	{"ze", "��", NULL},
	{"zo", "��", NULL},

	{"zya", "����", NULL},
	{"zyi", "����", NULL},
	{"zyu", "����", NULL},
	{"zye", "����", NULL},
	{"zyo", "����", NULL},

	{"z", "��", "z"},
    
	{"ja", "����", NULL},
	{"ji", "��", NULL},
	{"ju", "����", NULL},
	{"je", "����", NULL},
	{"jo", "����", NULL},

	{"jya", "����", NULL},
	{"jyi", "����", NULL},
	{"jyu", "����", NULL},
	{"jye", "����", NULL},
	{"jyo", "����", NULL},
    
	{"j", "��", "j"},
    
	{"ta", "��", NULL},
	{"ti", "��", NULL},
	{"tu", "��", NULL},
	{"te", "��", NULL},
	{"to", "��", NULL},

	{"tya", "����", NULL},
	{"tyi", "����", NULL},
	{"tyu", "����", NULL},
	{"tye", "����", NULL},
	{"tyo", "����", NULL},
    
	{"tha", "�Ƥ�", NULL},
	{"thi", "�Ƥ�", NULL},
	{"thu", "�Ƥ�", NULL},
	{"the", "�Ƥ�", NULL},
	{"tho", "�Ƥ�", NULL},

	{"t", "��", "tc"},

	{"cha", "����", NULL},
	{"chi", "��", NULL},
	{"chu", "����", NULL},
	{"che", "����", NULL},
	{"cho", "����", NULL},

	{"tsu", "��", NULL},
	{"xtu", "��", NULL},
	{"xtsu", "��", NULL},

	{"c", "��", "c"},

	{"da", "��", NULL},
	{"di", "��", NULL},
	{"du", "��", NULL},
	{"de", "��", NULL},
	{"do", "��", NULL},

	{"dya", "�¤�", NULL},
	{"dyi", "�¤�", NULL},
	{"dyu", "�¤�", NULL},
	{"dye", "�¤�", NULL},
	{"dyo", "�¤�", NULL},

	{"dha", "�Ǥ�", NULL},
	{"dhi", "�Ǥ�", NULL},
	{"dhu", "�Ǥ�", NULL},
	{"dhe", "�Ǥ�", NULL},
	{"dho", "�Ǥ�", NULL},
    
	{"d", "��", "d"},

	{"na", "��", NULL},
	{"ni", "��", NULL},
	{"nu", "��", NULL},
	{"ne", "��", NULL},
	{"no", "��", NULL},

	{"nya", "�ˤ�", NULL},
	{"nyi", "�ˤ�", NULL},
	{"nyu", "�ˤ�", NULL},
	{"nye", "�ˤ�", NULL},
	{"nyo", "�ˤ�", NULL},

	{"n", "��", NULL},
	{"nn", "��", NULL},

	{"ha", "��", NULL},
	{"hi", "��", NULL},
	{"hu", "��", NULL},
	{"he", "��", NULL},
	{"ho", "��", NULL},

	{"hya", "�Ҥ�", NULL},
	{"hyi", "�Ҥ�", NULL},
	{"hyu", "�Ҥ�", NULL},
	{"hye", "�Ҥ�", NULL},
	{"hyo", "�Ҥ�", NULL},

	{"h", "��", "h"},
    
	{"fa", "�դ�", NULL},
	{"fi", "�դ�", NULL},
	{"fu", "��", NULL},
	{"fe", "�դ�", NULL},
	{"fo", "�դ�", NULL},

	{"fya", "�դ�", NULL},
	{"fyi", "�դ�", NULL},
	{"fyu", "�դ�", NULL},
	{"fye", "�դ�", NULL},
	{"fyo", "�դ�", NULL},

	{"f", "��", "f"},
    
	{"ba", "��", NULL},
	{"bi", "��", NULL},
	{"bu", "��", NULL},
	{"be", "��", NULL},
	{"bo", "��", NULL},
    
	{"bya", "�Ӥ�", NULL},
	{"byi", "�Ӥ�", NULL},
	{"byu", "�Ӥ�", NULL},
	{"bye", "�Ӥ�", NULL},
	{"byo", "�Ӥ�", NULL},

	{"b", "��", "b" },

	{"pa", "��", NULL},
	{"pi", "��", NULL},
	{"pu", "��", NULL},
	{"pe", "��", NULL},
	{"po", "��", NULL},

	{"pya", "�Ԥ�", NULL},
	{"pyi", "�Ԥ�", NULL},
	{"pyu", "�Ԥ�", NULL},
	{"pye", "�Ԥ�", NULL},
	{"pyo", "�Ԥ�", NULL},
    
	{"p", "��", "p"},
    
	{"ma", "��", NULL},
	{"mi", "��", NULL},
	{"mu", "��", NULL},
	{"me", "��", NULL},
	{"mo", "��", NULL},

	{"mya", "�ߤ�", NULL},
	{"myi", "�ߤ�", NULL},
	{"myu", "�ߤ�", NULL},
	{"mye", "�ߤ�", NULL},
	{"myo", "�ߤ�", NULL},

	{"m", "��", "bp"},
	{"m", "��", "m"},

	{"y", "��", "y"},
	{"ya", "��", NULL},
	{"yu", "��", NULL},
	{"yo", "��", NULL},

	{"xya", "��", NULL},
	{"xyu", "��", NULL},
	{"xyo", "��", NULL},

	{"r", "��", "r"},
	{"ra", "��", NULL},
	{"ri", "��", NULL},
	{"ru", "��", NULL},
	{"re", "��", NULL},
	{"ro", "��", NULL},

	{"rya", "���", NULL},
	{"ryi", "�ꤣ", NULL},
	{"ryu", "���", NULL},
	{"rye", "�ꤧ", NULL},
	{"ryo", "���", NULL},

	{"xwa", "��", NULL},
	{"wa", "��", NULL},
	{"wi", "����", NULL},
	{"xwi", "��", NULL},
	{"we", "����", NULL},
	{"xwe", "��", NULL},
	{"wo", "��", NULL},
    
	{"va", "������", NULL},
	{"vi", "������", NULL},
	{"vu", "����", NULL},
	{"ve", "������", NULL},
	{"vo", "������", NULL},

	{NULL, NULL, NULL}
};

static const struct rk_rule rk_rule_katakana[] =
{
	SKK_LIKE_KIGO_MAP,

	{"a", "��", NULL},
	{"i", "��", NULL},
	{"u", "��", NULL},
	{"e", "��", NULL},
	{"o", "��", NULL},

	{"xa", "��", NULL},
	{"xi", "��", NULL},
	{"xu", "��", NULL},
	{"xe", "��", NULL},
	{"xo", "��", NULL},
	
	{"ka", "��", NULL},
	{"ki", "��", NULL},
	{"ku", "��", NULL},
	{"ke", "��", NULL},
	{"ko", "��", NULL},

	{"kya", "����", NULL},
	{"kyi", "����", NULL},
	{"kyu", "����", NULL},
	{"kye", "����", NULL},
	{"kyo", "����", NULL},
    
	{"k", "��", "k"},

	{"ga", "��", NULL},
	{"gi", "��", NULL},
	{"gu", "��", NULL},
	{"ge", "��", NULL},
	{"go", "��", NULL},

	{"gya", "����", NULL},
	{"gyi", "����", NULL},
	{"gyu", "����", NULL},
	{"gye", "����", NULL},
	{"gyo", "����", NULL},
    
	{"g", "��", "g"},

	{"sa", "��", NULL},
	{"si", "��", NULL},
	{"su", "��", NULL},
	{"se", "��", NULL},
	{"so", "��", NULL},

	{"sya", "����", NULL},
	{"syi", "����", NULL},
	{"syu", "����", NULL},
	{"sye", "����", NULL},
	{"syo", "����", NULL},
    
	{"sha", "����", NULL},
	{"shi", "��", NULL},
	{"shu", "����", NULL},
	{"she", "����", NULL},
	{"sho", "����", NULL},

	{"s", "��", "s"},

	{"za", "��", NULL},
	{"zi", "��", NULL},
	{"zu", "��", NULL},
	{"ze", "��", NULL},
	{"zo", "��", NULL},

	{"zya", "����", NULL},
	{"zyi", "����", NULL},
	{"zyu", "����", NULL},
	{"zye", "����", NULL},
	{"zyo", "����", NULL},

	{"z", "��", "z"},
    
	{"ja", "����", NULL},
	{"ji", "��", NULL},
	{"ju", "����", NULL},
	{"je", "����", NULL},
	{"jo", "����", NULL},

	{"jya", "����", NULL},
	{"jyi", "����", NULL},
	{"jyu", "����", NULL},
	{"jye", "����", NULL},
	{"jyo", "����", NULL},
    
	{"j", "��", "j"},
    
	{"ta", "��", NULL},
	{"ti", "��", NULL},
	{"tu", "��", NULL},
	{"te", "��", NULL},
	{"to", "��", NULL},

	{"tya", "����", NULL},
	{"tyi", "����", NULL},
	{"tyu", "����", NULL},
	{"tye", "����", NULL},
	{"tyo", "����", NULL},

	{"tha", "�ƥ�", NULL},
	{"thi", "�ƥ�", NULL},
	{"thu", "�ƥ�", NULL},
	{"the", "�ƥ�", NULL},
	{"tho", "�ƥ�", NULL},

	{"t", "��", "tc"},

	{"cha", "����", NULL},
	{"chi", "��", NULL},
	{"chu", "����", NULL},
	{"che", "����", NULL},
	{"cho", "����", NULL},

	{"tsu", "��", NULL},
	{"xtu", "��", NULL},
	{"xtsu", "��", NULL},

	{"c", "��", "c"},

	{"da", "��", NULL},
	{"di", "��", NULL},
	{"du", "��", NULL},
	{"de", "��", NULL},
	{"do", "��", NULL},

	{"dya", "�¥�", NULL},
	{"dyi", "�¥�", NULL},
	{"dyu", "�¥�", NULL},
	{"dye", "�¥�", NULL},
	{"dyo", "�¥�", NULL},

	{"dha", "�ǥ�", NULL},
	{"dhi", "�ǥ�", NULL},
	{"dhu", "�ǥ�", NULL},
	{"dhe", "�ǥ�", NULL},
	{"dho", "�ǥ�", NULL},
    
	{"d", "��", "d"},

	{"na", "��", NULL},
	{"ni", "��", NULL},
	{"nu", "��", NULL},
	{"ne", "��", NULL},
	{"no", "��", NULL},

	{"nya", "�˥�", NULL},
	{"nyi", "�˥�", NULL},
	{"nyu", "�˥�", NULL},
	{"nye", "�˥�", NULL},
	{"nyo", "�˥�", NULL},

	{"n", "��", NULL},
	{"nn", "��", NULL},

	{"ha", "��", NULL},
	{"hi", "��", NULL},
	{"hu", "��", NULL},
	{"he", "��", NULL},
	{"ho", "��", NULL},

	{"hya", "�ҥ�", NULL},
	{"hyi", "�ҥ�", NULL},
	{"hyu", "�ҥ�", NULL},
	{"hye", "�ҥ�", NULL},
	{"hyo", "�ҥ�", NULL},

	{"h", "��", "h"},
    
	{"fa", "�ե�", NULL},
	{"fi", "�ե�", NULL},
	{"fu", "��", NULL},
	{"fe", "�ե�", NULL},
	{"fo", "�ե�", NULL},

	{"fya", "�ե�", NULL},
	{"fyi", "�ե�", NULL},
	{"fyu", "�ե�", NULL},
	{"fye", "�ե�", NULL},
	{"fyo", "�ե�", NULL},

	{"f", "��", "f"},
    
	{"ba", "��", NULL},
	{"bi", "��", NULL},
	{"bu", "��", NULL},
	{"be", "��", NULL},
	{"bo", "��", NULL},
    
	{"bya", "�ӥ�", NULL},
	{"byi", "�ӥ�", NULL},
	{"byu", "�ӥ�", NULL},
	{"bye", "�ӥ�", NULL},
	{"byo", "�ӥ�", NULL},

	{"b", "��", NULL},

	{"pa", "��", NULL},
	{"pi", "��", NULL},
	{"pu", "��", NULL},
	{"pe", "��", NULL},
	{"po", "��", NULL},

	{"pya", "�ԥ�", NULL},
	{"pyi", "�ԥ�", NULL},
	{"pyu", "�ԥ�", NULL},
	{"pye", "�ԥ�", NULL},
	{"pyo", "�ԥ�", NULL},
    
	{"p", "��", "p"},
    
	{"ma", "��", NULL},
	{"mi", "��", NULL},
	{"mu", "��", NULL},
	{"me", "��", NULL},
	{"mo", "��", NULL},

	{"mya", "�ߥ�", NULL},
	{"myi", "�ߥ�", NULL},
	{"myu", "�ߥ�", NULL},
	{"mye", "�ߥ�", NULL},
	{"myo", "�ߥ�", NULL},

	{"m", "��", "bp"},

	{"y", "��", "y"},
	{"ya", "��", NULL},
	{"yu", "��", NULL},
	{"yo", "��", NULL},

	{"xya", "��", NULL},
	{"xyu", "��", NULL},
	{"xyo", "��", NULL},

	{"r", "��", "r"},
	{"ra", "��", NULL},
	{"ri", "��", NULL},
	{"ru", "��", NULL},
	{"re", "��", NULL},
	{"ro", "��", NULL},

	{"rya", "���", NULL},
	{"ryi", "�ꥣ", NULL},
	{"ryu", "���", NULL},
	{"rye", "�ꥧ", NULL},
	{"ryo", "���", NULL},

	{"xwa", "��", NULL},
	{"wa", "��", NULL},
	{"wi", "����", NULL},
	{"xwi", "��", NULL},
	{"we", "����", NULL},
	{"xwe", "��", NULL},
	{"wo", "��", NULL},
    
	{"va", "����", NULL},
	{"vi", "����", NULL},
	{"vu", "��", NULL},
	{"ve", "����", NULL},
	{"vo", "����", NULL},

	{NULL, NULL, NULL}
};

/*
 * Local variables:
 *  c-indent-level: 8
 *  c-basic-offset: 8
 * End:
 */
