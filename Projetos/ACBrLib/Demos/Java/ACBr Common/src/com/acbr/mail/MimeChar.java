package com.acbr.mail;



import java.util.HashMap;
import java.util.Map;

public enum MimeChar {
    ISO_8859_1(0),
    ISO_8859_2(1),
    ISO_8859_3(2),
    ISO_8859_4(3),
    ISO_8859_5(4),
    ISO_8859_6(5),
    ISO_8859_7(6),
    ISO_8859_8(7),
    ISO_8859_9(8),
    ISO_8859_10(9),
    ISO_8859_13(10),
    ISO_8859_14(11),
    ISO_8859_15(12),
    CP1250(13),
    CP1251(14),
    CP1252(15),
    CP1253(16),
    CP1254(17),
    CP1255(18),
    CP1256(19),
    CP1257(20),
    CP1258(21),
    KOI8_R(22),
    CP895(23),
    CP852(24),
    UCS_2(25),
    UCS_4(26),
    UTF_8(27),
    UTF_7(28),
    UTF_7mod(29),
    UCS_2LE(30),
    UCS_4LE(31),
    UTF_16(32),
    UTF_16LE(33),
    UTF_32(34),
    UTF_32LE(35),
    C99(36),
    JAVA(37),
    ISO_8859_16(38),
    KOI8_U(39),
    KOI8_RU(40),
    CP862(41),
    CP866(42),
    MAC(43),
    MACCE(44),
    MACICE(45),
    MACCRO(46),
    MACRO(47),
    MACCYR(48),
    MACUK(49),
    MACGR(50),
    MACTU(51),
    MACHEB(52),
    MACAR(53),
    MACTH(54),
    ROMAN8(55),
    NEXTSTEP(56),
    ARMASCII(57),
    GEORGIAN_AC(58),
    GEORGIAN_PS(59),
    KOI8_T(60),
    MULELAO(61),
    CP1133(62),
    TIS620(63),
    CP874(64),
    VISCII(65),
    TCVN(66),
    ISO_IR_14(67),
    JIS_X0201(68),
    JIS_X0208(69),
    JIS_X0212(70),
    GB1988_80(71),
    GB2312_80(72),
    ISO_IR_165(73),
    ISO_IR_149(74),
    EUC_JP(75),
    SHIFT_JIS(76),
    CP932(77),
    ISO_2022_JP(78),
    ISO_2022_JP1(79),
    ISO_2022_JP2(80),
    GB2312(81),
    CP936(82),
    GB18030(83),
    ISO_2022_CN(84),
    ISO_2022_CNE(85),
    HZ(86),
    EUC_TW(87),
    BIG5(88),
    CP950(89),
    BIG5_HKSCS(90),
    EUC_KR(91),
    CP949(92),
    CP1361(93),
    ISO_2022_KR(94),
    CP737(95),
    CP775(96),
    CP853(97),
    CP855(98),
    CP857(99),
    CP858(100),
    CP860(101),
    CP861(102),
    CP863(103),
    CP864(104),
    CP865(105),
    CP869(106),
    CP1125(107);

    private static final Map<Integer, MimeChar> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (MimeChar value : MimeChar.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static MimeChar valueOf(int value) {
        return map.get(value);
    }

    private MimeChar(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
