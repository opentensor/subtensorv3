

use super::*;

/// Returns the Unicode symbol as a Vec<u8> for a given netuid.
impl<T: Config> Pallet<T> {
        pub fn get_name_for_subnet(netuid: u16) -> Vec<u8> {

                if SubnetName::<T>::contains_key(netuid) {
                        SubnetName::<T>::get(netuid)
                } else{
                        match netuid {
                                // Greek Alphabet (Lowercase)
                                0 => b"root".to_vec(), // Τ (Upper case Tau)
                                1 => b"apex".to_vec(), // α (Alpha)
                                2 => b"omron".to_vec(), // β (Beta)
                                3 => b"templar".to_vec(), // γ (Gamma)
                                4 => b"targon".to_vec(), // δ (Delta)
                                5 => b"kaito".to_vec(), // ε (Epsilon)
                                6 => b"infinite".to_vec(), // ζ (Zeta)
                                7 => b"subvortex".to_vec(), // η (Eta)
                                8 => b"ptn".to_vec(), // θ (Theta)
                                9 => b"pretrain".to_vec(), // ι (Iota)
                                10 => b"sturdy".to_vec(), // κ (Kappa)
                                11 => b"dippy".to_vec(), // λ (Lambda)
                                12 => b"horde".to_vec(), // μ (Mu)
                                13 => b"dataverse".to_vec(), // ν (Nu)
                                14 => b"palaidn".to_vec(), // ξ (Xi)
                                15 => b"deval".to_vec(), // ο (Omicron)
                                16 => b"bitads".to_vec(), // π (Pi)
                                17 => b"3gen".to_vec(), // ρ (Rho)
                                18 => b"cortex".to_vec(), // σ (Sigma)
                                19 => b"inference".to_vec(), // t (Tau)
                                20 => b"bitagent".to_vec(), // υ (Upsilon)
                                21 => b"any-any".to_vec(), // φ (Phi)
                                22 => b"meta".to_vec(), // χ (Chi)
                                23 => b"social".to_vec(), // ψ (Psi)
                                24 => b"omega".to_vec(), // ω (Omega)
                                25 => b"protein".to_vec(), // א (Aleph)
                                26 => b"alchemy".to_vec(), // ב (Bet)
                                27 => b"compute".to_vec(), // ג (Gimel)
                                28 => b"oracle".to_vec(), // ד (Dalet)
                                29 => b"coldint".to_vec(), // ה (He)
                                30 => b"bet".to_vec(), // ו (Vav)
                                31 => b"naschain".to_vec(), // ז (Zayin)
                                32 => b"itsai".to_vec(), // ח (Het)
                                33 => b"ready".to_vec(), // ט (Tet)
                                34 => b"mind".to_vec(), // י (Yod)
                                35 => b"logic".to_vec(), // ך (Final Kaf)
                                36 => b"automata".to_vec(), // כ (Kaf)
                                37 => b"tuning".to_vec(), // ל (Lamed)
                                38 => b"distributed".to_vec(), // ם (Final Mem)
                                39 => b"edge".to_vec(), // מ (Mem)
                                40 => b"chunk".to_vec(), // ן (Final Nun)
                                41 => b"sportsensor".to_vec(), // נ (Nun)
                                42 => b"masa".to_vec(), // ס (Samekh)
                                43 => b"graphite".to_vec(), // ע (Ayin)
                                44 => b"score".to_vec(), // ף (Final Pe)
                                45 => b"gen42".to_vec(), // פ (Pe)
                                46 => b"neural".to_vec(), // ץ (Final Tsadi)
                                47 => b"condense".to_vec(), // צ (Tsadi)
                                48 => b"nextplace".to_vec(), // ק (Qof)
                                49 => b"automl".to_vec(), // ר (Resh)
                                50 => b"audio".to_vec(), // ש (Shin)
                                51 => b"celium".to_vec(), // ת (Tav)

                                52 => b"dojo".to_vec(), // ا (Alif)
                                53 => b"frontier".to_vec(), // ب (Ba)
                                54 => b"safescan".to_vec(), // ت (Ta)
                                55 => b"unknown".to_vec(), // ث (Tha)
                                56 => b"gradients".to_vec(), // ج (Jim)
                                57 => b"gaia".to_vec(), // ح (Ha)
                                58 => b"dippy-speach".to_vec(), // خ (Kha)
                                59 => b"agent-arena".to_vec(), // د (Dal)
                                60 => b"unknown".to_vec(), // ذ (Dhal)
                                61 => b"red team".to_vec(), // ر (Ra)
                                62 => b"agentao".to_vec(), // ز (Zay)
                                63 => b"lean-in".to_vec(), // س (Sin)
                                64 => b"chutes".to_vec(), // ش (Shin)
                                // Default case
                                _ => b"unknown".to_vec(), // unknown subnet.
                        }
                }
        }

        pub fn get_symbol_for_subnet(netuid: u16) -> Vec<u8> {
                match netuid {
                        // Greek Alphabet (Lowercase)
                        0 => b"\xCE\xA4".to_vec(), // Τ (Upper case Tau)
                        1 => b"\xCE\xB1".to_vec(), // α (Alpha)
                        2 => b"\xCE\xB2".to_vec(), // β (Beta)
                        3 => b"\xCE\xB3".to_vec(), // γ (Gamma)
                        4 => b"\xCE\xB4".to_vec(), // δ (Delta)
                        5 => b"\xCE\xB5".to_vec(), // ε (Epsilon)
                        6 => b"\xCE\xB6".to_vec(), // ζ (Zeta)
                        7 => b"\xCE\xB7".to_vec(), // η (Eta)
                        8 => b"\xCE\xB8".to_vec(), // θ (Theta)
                        9 => b"\xCE\xB9".to_vec(), // ι (Iota)
                        10 => b"\xCE\xBA".to_vec(), // κ (Kappa)
                        11 => b"\xCE\xBB".to_vec(), // λ (Lambda)
                        12 => b"\xCE\xBC".to_vec(), // μ (Mu)
                        13 => b"\xCE\xBD".to_vec(), // ν (Nu)
                        14 => b"\xCE\xBE".to_vec(), // ξ (Xi)
                        15 => b"\xCE\xBF".to_vec(), // ο (Omicron)
                        16 => b"\xCF\x80".to_vec(), // π (Pi)
                        17 => b"\xCF\x81".to_vec(), // ρ (Rho)
                        18 => b"\xCF\x83".to_vec(), // σ (Sigma)
                        19 => b"t".to_vec(), // t (Tau)
                        20 => b"\xCF\x85".to_vec(), // υ (Upsilon)
                        21 => b"\xCF\x86".to_vec(), // φ (Phi)
                        22 => b"\xCF\x87".to_vec(), // χ (Chi)
                        23 => b"\xCF\x88".to_vec(), // ψ (Psi)
                        24 => b"\xCF\x89".to_vec(), // ω (Omega)
                        // Hebrew Alphabet (Including Final Forms)
                        25 => b"\xD7\x90".to_vec(), // א (Aleph)
                        26 => b"\xD7\x91".to_vec(), // ב (Bet)
                        27 => b"\xD7\x92".to_vec(), // ג (Gimel)
                        28 => b"\xD7\x93".to_vec(), // ד (Dalet)
                        29 => b"\xD7\x94".to_vec(), // ה (He)
                        30 => b"\xD7\x95".to_vec(), // ו (Vav)
                        31 => b"\xD7\x96".to_vec(), // ז (Zayin)
                        32 => b"\xD7\x97".to_vec(), // ח (Het)
                        33 => b"\xD7\x98".to_vec(), // ט (Tet)
                        34 => b"\xD7\x99".to_vec(), // י (Yod)
                        35 => b"\xD7\x9A".to_vec(), // ך (Final Kaf)
                        36 => b"\xD7\x9B".to_vec(), // כ (Kaf)
                        37 => b"\xD7\x9C".to_vec(), // ל (Lamed)
                        38 => b"\xD7\x9D".to_vec(), // ם (Final Mem)
                        39 => b"\xD7\x9E".to_vec(), // מ (Mem)
                        40 => b"\xD7\x9F".to_vec(), // ן (Final Nun)
                        41 => b"\xD7\xA0".to_vec(), // נ (Nun)
                        42 => b"\xD7\xA1".to_vec(), // ס (Samekh)
                        43 => b"\xD7\xA2".to_vec(), // ע (Ayin)
                        44 => b"\xD7\xA3".to_vec(), // ף (Final Pe)
                        45 => b"\xD7\xA4".to_vec(), // פ (Pe)
                        46 => b"\xD7\xA5".to_vec(), // ץ (Final Tsadi)
                        47 => b"\xD7\xA6".to_vec(), // צ (Tsadi)
                        48 => b"\xD7\xA7".to_vec(), // ק (Qof)
                        49 => b"\xD7\xA8".to_vec(), // ר (Resh)
                        50 => b"\xD7\xA9".to_vec(), // ש (Shin)
                        51 => b"\xD7\xAA".to_vec(), // ת (Tav)

                        // Arabic Alphabet
                        52 => b"\xD8\xA7".to_vec(), // ا (Alif)
                        53 => b"\xD8\xA8".to_vec(), // ب (Ba)
                        54 => b"\xD8\xAA".to_vec(), // ت (Ta)
                        55 => b"\xD8\xAB".to_vec(), // ث (Tha)
                        56 => b"\xD8\xAC".to_vec(), // ج (Jim)
                        57 => b"\xD8\xAD".to_vec(), // ح (Ha)
                        58 => b"\xD8\xAE".to_vec(), // خ (Kha)
                        59 => b"\xD8\xAF".to_vec(), // د (Dal)
                        60 => b"\xD8\xB0".to_vec(), // ذ (Dhal)
                        61 => b"\xD8\xB1".to_vec(), // ر (Ra)
                        62 => b"\xD8\xB2".to_vec(), // ز (Zay)
                        63 => b"\xD8\xB3".to_vec(), // س (Sin)
                        64 => b"\xD8\xB4".to_vec(), // ش (Shin)
                        65 => b"\xD8\xB5".to_vec(), // ص (Sad)
                        66 => b"\xD8\xB6".to_vec(), // ض (Dad)
                        67 => b"\xD8\xB7".to_vec(), // ط (Ta)
                        68 => b"\xD8\xB8".to_vec(), // ظ (Dha)
                        69 => b"\xD8\xB9".to_vec(), // ع (Ain)
                        70 => b"\xD8\xBA".to_vec(), // غ (Ghayn)
                        71 => b"\xD9\x81".to_vec(), // ف (Fa)
                        72 => b"\xD9\x82".to_vec(), // ق (Qaf)
                        73 => b"\xD9\x83".to_vec(), // ك (Kaf)
                        74 => b"\xD9\x84".to_vec(), // ل (Lam)
                        75 => b"\xD9\x85".to_vec(), // م (Mim)
                        76 => b"\xD9\x86".to_vec(), // ن (Nun)
                        77 => b"\xD9\x87".to_vec(), // ه (Ha)
                        78 => b"\xD9\x88".to_vec(), // و (Waw)
                        79 => b"\xD9\x8A".to_vec(), // ي (Ya)
                        80 => b"\xD9\x89".to_vec(), // ى (Alef Maksura, 80)
                        81 => b"\xD9\x8A".to_vec(), // ي (Ya, 81)

                        // Runic Alphabet
                        82 => b"\xE1\x9A\xA0".to_vec(), // ᚠ (Fehu, wealth, 82)
                        83 => b"\xE1\x9A\xA2".to_vec(), // ᚢ (Uruz, strength, 83)
                        84 => b"\xE1\x9A\xA6".to_vec(), // ᚦ (Thurisaz, giant, 84)
                        85 => b"\xE1\x9A\xA8".to_vec(), // ᚨ (Ansuz, god, 85)
                        86 => b"\xE1\x9A\xB1".to_vec(), // ᚱ (Raidho, ride, 86)
                        87 => b"\xE1\x9A\xB3".to_vec(), // ᚲ (Kaunan, ulcer, 87)
                        88 => b"\xE1\x9B\x87".to_vec(), // ᛇ (Eihwaz, yew, 88)
                        89 => b"\xE1\x9B\x89".to_vec(), // ᛉ (Algiz, protection, 89)
                        90 => b"\xE1\x9B\x92".to_vec(), // ᛒ (Berkanan, birch, 90)

                        // Ogham Alphabet
                        91 => b"\xE1\x9A\x80".to_vec(), //   (Space, 91)
                        92 => b"\xE1\x9A\x81".to_vec(), // ᚁ (Beith, birch, 92)
                        93 => b"\xE1\x9A\x82".to_vec(), // ᚂ (Luis, rowan, 93)
                        94 => b"\xE1\x9A\x83".to_vec(), // ᚃ (Fearn, alder, 94)
                        95 => b"\xE1\x9A\x84".to_vec(), // ᚄ (Sail, willow, 95)
                        96 => b"\xE1\x9A\x85".to_vec(), // ᚅ (Nion, ash, 96)
                        97 => b"\xE1\x9A\x9B".to_vec(), // ᚛ (Forfeda, 97)

                        // Georgian Alphabet (Mkhedruli)
                        98 => b"\xE1\x83\x90".to_vec(), // ა (Ani, 98)
                        99 => b"\xE1\x83\x91".to_vec(), // ბ (Bani, 99)
                        100 => b"\xE1\x83\x92".to_vec(), // გ (Gani, 100)
                        101 => b"\xE1\x83\x93".to_vec(), // დ (Doni, 101)
                        102 => b"\xE1\x83\x94".to_vec(), // ე (Eni, 102)
                        103 => b"\xE1\x83\x95".to_vec(), // ვ (Vini, 103)

                        // Armenian Alphabet
                        104 => b"\xD4\xB1".to_vec(), // Ա (Ayp, 104)
                        105 => b"\xD4\xB2".to_vec(), // Բ (Ben, 105)
                        106 => b"\xD4\xB3".to_vec(), // Գ (Gim, 106)
                        107 => b"\xD4\xB4".to_vec(), // Դ (Da, 107)
                        108 => b"\xD4\xB5".to_vec(), // Ե (Ech, 108)
                        109 => b"\xD4\xB6".to_vec(), // Զ (Za, 109)
                        110 => b"\xD5\x9E".to_vec(), // ՞ (Question mark, 110)

                        // Cyrillic Alphabet
                        111 => b"\xD0\x80".to_vec(), // Ѐ (Ie with grave, 111)
                        112 => b"\xD0\x81".to_vec(), // Ё (Io, 112)
                        113 => b"\xD0\x82".to_vec(), // Ђ (Dje, 113)
                        114 => b"\xD0\x83".to_vec(), // Ѓ (Gje, 114)
                        115 => b"\xD0\x84".to_vec(), // Є (Ukrainian Ie, 115)
                        116 => b"\xD0\x85".to_vec(), // Ѕ (Dze, 116)

                        // Coptic Alphabet
                        117 => b"\xE2\xB2\x80".to_vec(), // Ⲁ (Alfa, 117)
                        118 => b"\xE2\xB2\x81".to_vec(), // ⲁ (Small Alfa, 118)
                        119 => b"\xE2\xB2\x82".to_vec(), // Ⲃ (Vida, 119)
                        120 => b"\xE2\xB2\x83".to_vec(), // ⲃ (Small Vida, 120)
                        121 => b"\xE2\xB2\x84".to_vec(), // Ⲅ (Gamma, 121)
                        122 => b"\xE2\xB2\x85".to_vec(), // ⲅ (Small Gamma, 122)

                        // Brahmi Script
                        123 => b"\xF0\x91\x80\x80".to_vec(), // 𑀀 (A, 123)
                        124 => b"\xF0\x91\x80\x81".to_vec(), // 𑀁 (Aa, 124)
                        125 => b"\xF0\x91\x80\x82".to_vec(), // 𑀂 (I, 125)
                        126 => b"\xF0\x91\x80\x83".to_vec(), // 𑀃 (Ii, 126)
                        127 => b"\xF0\x91\x80\x85".to_vec(), // 𑀅 (U, 127)

                        // Tifinagh Alphabet
                        128 => b"\xE2\xB4\xB0".to_vec(), // ⴰ (Ya, 128)
                        129 => b"\xE2\xB4\xB1".to_vec(), // ⴱ (Yab, 129)
                        130 => b"\xE2\xB4\xB2".to_vec(), // ⴲ (Yabh, 130)
                        131 => b"\xE2\xB4\xB3".to_vec(), // ⴳ (Yag, 131)
                        132 => b"\xE2\xB4\xB4".to_vec(), // ⴴ (Yagh, 132)
                        133 => b"\xE2\xB4\xB5".to_vec(), // ⴵ (Yaj, 133)

                        // Default case
                        _ => b"\xCE\xA4".to_vec(), // Default to TAO uppercase symbol.
                }
        }
}