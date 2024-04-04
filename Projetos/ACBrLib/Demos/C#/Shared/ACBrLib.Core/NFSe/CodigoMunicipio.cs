using System;
using System.ComponentModel;

namespace ACBrLib.Core.NFSe
{
    public enum CodigoMunicipio
    {
        [EnumValue("0")]
        [Description("Nenhuma Cidade Selecionada")]
        NenhumaCidadeSelecionada = 0,

        //Rondônia
        [EnumValue("1100023")]
        [Description("Ariquemes - RO")]
        Ariquemes = 1100023,

        [EnumValue("1100031")]
        [Description("Cabixi - RO")]
        Cabixi = 1100031,

        [EnumValue("1100049")]
        [Description("Cacoal - RO")]
        Cacoal = 1100049,

        [EnumValue("1100064")]
        [Description("Colorado do Oeste - RO")]
        ColoradodoOeste = 1100064,

        [EnumValue("1100106")]
        [Description("Guajara-Mirim - RO")]
        GuajaraMirim = 1100106,

        [EnumValue("1100114")]
        [Description("Jaru - RO")]
        Jaru = 1100114,

        [EnumValue("1100122")]
        [Description("Ji-Parana - RO")]
        JiParana = 1100122,

        [EnumValue("1100130")]
        [Description("Machadinho do Oeste - RO")]
        MachadinhodoOeste = 1100130,

        [EnumValue("1100155")]
        [Description("Ouro Preto do Oeste - RO")]
        OuroPretodoOeste = 1100155,

        [EnumValue("1100189")]
        [Description("Pimenta Bueno - RO")]
        PimentaBuendo = 1100189,

        [EnumValue("1100205")]
        [Description("Porto Velho - RO")]
        PortoVelho = 1100205,

        [EnumValue("1100254")]
        [Description("Presidente Medici - RO")]
        PresidenteMedici = 1100254,

        [EnumValue("1100304")]
        [Description("Vilhena - RO")]
        Vilhena = 1100304,

        [EnumValue("1100403")]
        [Description("Alto Paraiso - RO")]
        AltoParaiso = 1100403,

        [EnumValue("1100452")]
        [Description("Buritis - RO")]
        Buritis = 1100452,

        [EnumValue("1100940")]
        [Description("Cujubim - RO")]
        Cujubim = 1100940,

        [EnumValue("1101104")]
        [Description("Itapuã do Oeste - RO")]
        ItapuadoOeste = 1101104,

        [EnumValue("1101757")]
        [Description("Vale do Anari - RO")]
        ValedoAnari = 1101757,

        //Acre
        [EnumValue("1200401")]
        [Description("Rio Branco - AC")]
        RioBranco = 1200401,

        [EnumValue("1200427")]
        [Description("Rodrigues Alves - AC")]
        RodriguesAlves = 1200427,

        [EnumValue("1200450")]
        [Description("Senador Guiomard - AC")]
        SenadorGuiomard = 1200450,

        [EnumValue("1200609")]
        [Description("Tarauaca - AC")]
        Tarauaca = 1200609,

        //Amazonas
        [EnumValue("1301852")]
        [Description("Iranduba - AM")]
        Iranduba = 1301852,

        [EnumValue("1302603")]
        [Description("Manaus - AM")]
        Manaus = 1302603,

        //Roraima
        [EnumValue("1400100")]
        [Description("Boa Vista - RR")]
        BoaVista = 1400100,

        [EnumValue("1400159")]
        [Description("Bonfim - RR")]
        Bonfim = 1400159,

        [EnumValue("1400209")]
        [Description("Caracarai - RR")]
        Caracarai = 1400209,

        //Pará
        [EnumValue("1500602")]
        [Description("Altamira - PA")]
        Altamira = 1500602,

        [EnumValue("1500800")]
        [Description("Ananindeua - PA")]
        Ananindeua = 1500800,

        [EnumValue("1500958")]
        [Description("Aurora do Para - PA")]
        AuroradoPara = 1500958,

        [EnumValue("1501303")]
        [Description("Barcarena - PA")]
        Barcarena = 1501303,

        [EnumValue("1501402")]
        [Description("Belem - PA")]
        Belem = 1501402,

        [EnumValue("1501451")]
        [Description("Belterra - PA")]
        Belterra = 1501451,

        [EnumValue("1502707")]
        [Description("Conceicao do Araguaia - PA")]
        ConceicaodoAraguaia = 1502707,

        [EnumValue("1503606")]
        [Description("Itaituba - PA")]
        Itaituba = 1503606,

        [EnumValue("1504208")]
        [Description("Maraba - PA")]
        Maraba = 1504208,

        [EnumValue("1504422")]
        [Description("Marituba - PA")]
        Marituba = 1504422,

        [EnumValue("1505031")]
        [Description("Novo Progresso - PA")]
        NovoProgresso = 1505031,

        [EnumValue("1505304")]
        [Description("Oriximina - PA")]
        Oriximina = 1505304,

        [EnumValue("1505536")]
        [Description("Parauapebas - PA")]
        Parauapebas = 1505536,

        [EnumValue("1506138")]
        [Description("Redencao - PA")]
        Redencao = 1506138,

        [EnumValue("1506161")]
        [Description("Rio Maria - PA")]
        RioMaria = 1506161,

        [EnumValue("1506807")]
        [Description("Santarem - PA")]
        SantaremPA = 1506807,

        [EnumValue("1507003")]
        [Description("Santo Antonio do Taua - PA")]
        SantoAntoniodoTaua = 1507003,

        [EnumValue("1507102")]
        [Description("Sao Caetano de Odivelas - PA")]
        SaoCaetanodeOdivelas = 1507102,

        [EnumValue("1507300")]
        [Description("Sao Felix do Xingu - PA")]
        SaoFelixdoXingu = 1507300,

        [EnumValue("1508084")]
        [Description("Tucuma - PA")]
        Tucuma = 1508084,

        [EnumValue("1508407")]
        [Description("Xinguara - PA")]
        Xinguara = 1508407,

        [EnumValue("1600303")]
        [Description("Macapa - AP")]
        Macapa = 1600303,

        //Tocantins
        [EnumValue("1702109")]
        [Description("Araguaína - TO")]
        Araguaina = 1702109,

        [EnumValue("1705508")]
        [Description("Colinas do Tocantins - TO")]
        ColinasdoTocantins = 1705508,

        [EnumValue("1709302")]
        [Description("Guarai - TO")]
        Guarai = 1709302,

        [EnumValue("1709500")]
        [Description("Gurupi - TO")]
        Gurupi = 1709500,

        [EnumValue("1711902")]
        [Description("Lagoa da Confusao - TO")]
        LagoadaConfusao = 1711902,

        [EnumValue("1713304")]
        [Description("Miranorte - TO")]
        Miranorte = 1713304,

        [EnumValue("1716109")]
        [Description("Paraiso do Tocantins - TO")]
        ParaisodoTocantins = 1716109,

        [EnumValue("1718204")]
        [Description("Porto Nacional - TO")]
        PortoNacional = 1718204,

        [EnumValue("1721000")]
        [Description("Palmas - TO")]
        PalmasTO = 1721000,

        [EnumValue("1722107")]
        [Description("Xambioa - TO")]
        Xambioa = 1722107,

        //Maranhão
        [EnumValue("2100055")]
        [Description("Açailandia - MA")]
        Acailandia = 2100055,

        [EnumValue("2100204")]
        [Description("Alcantara - MA")]
        Alcantara = 2100204,

        [EnumValue("2100501")]
        [Description("Alto Parnaiba - MA")]
        AltoParnaiba = 2100501,

        [EnumValue("2101202")]
        [Description("Bacabal - MA")]
        Bacabal = 2101202,

        [EnumValue("2101400")]
        [Description("Balsas - MA")]
        Balsas = 2101400,

        [EnumValue("2103000")]
        [Description("Caxias - MA")]
        Caxias = 2103000,

        [EnumValue("2104800")]
        [Description("Grajau - MA")]
        Grajau = 2104800,

        [EnumValue("2105302")]
        [Description("Imperatriz - MA")]
        Imperatriz = 2105302,

        [EnumValue("2108207")]
        [Description("Pedreiras - MA")]
        Pedreiras = 2108207,

        [EnumValue("2110005")]
        [Description("Santa Luzia - MA")]
        SantaLuziaMA = 2110005,

        [EnumValue("2111300")]
        [Description("Sao Luis - MA")]
        SaoLuis = 2111300,

        [EnumValue("2112209")]
        [Description("Timon - MA")]
        Timon = 2112209,

        //Piauí
        [EnumValue("2205706")]
        [Description("Luís Correia - PI")]
        LuisCorreia = 2205706,

        [EnumValue("2207009")]
        [Description("Oeiras - PI")]
        Oeiras = 2207009,

        [EnumValue("2207702")]
        [Description("Parnaíba - PI")]
        Parnaiba = 2207702,

        [EnumValue("2208007")]
        [Description("Picos - PI")]
        Picos = 2208007,

        [EnumValue("2211001")]
        [Description("Teresina - PI")]
        Teresina = 2211001,

        [EnumValue("2211209")]
        [Description("Urucui - PI")]
        Urucui = 2211209,

        [EnumValue("2211308")]
        [Description("Valenca do Piaui - PI")]
        ValencadoPiaui = 2211308,

        //Ceará
        [EnumValue("2301000")]
        [Description("Aquiraz - CE")]
        Aquiraz = 2301000,

        [EnumValue("2301109")]
        [Description("Aracati - CE")]
        Aracati = 2301109,

        [EnumValue("2301901")]
        [Description("Barbalha - CE")]
        Barbalha = 2301901,

        [EnumValue("2302206")]
        [Description("Beberibe - CE")]
        Beberibe = 2302206,

        [EnumValue("2304103")]
        [Description("Crateus - CE")]
        Crateus = 2304103,

        [EnumValue("2304202")]
        [Description("Crato - CE")]
        Crato = 2304202,

        [EnumValue("2304285")]
        [Description("Eusebio - CE")]
        Eusebio = 2304285,

        [EnumValue("2304400")]
        [Description("Fortaleza - CE")]
        Fortaleza = 2304400,

        [EnumValue("2305001")]
        [Description("Guaraciaba do Norte - CE")]
        GuaraciabadoNorte = 2305001,

        [EnumValue("2306256")]
        [Description("Itaitinga - CE")]
        Itaitinga = 2306256,

        [EnumValue("2307254")]
        [Description("Jijoca de Jericoacoara - CE")]
        JijocadeJericoacoara = 2307254,

        [EnumValue("2307304")]
        [Description("Juazeiro do Norte - CE")]
        JuazeirodoNorte = 2307304,

        [EnumValue("2307650")]
        [Description("Maracanau - CE")]
        Maracanau = 2307650,

        [EnumValue("2310852")]
        [Description("Pindoretama - CE")]
        Pindoretama = 2310852,

        [EnumValue("2311405")]
        [Description("Quixeramobim - CE")]
        Quixeramobim = 2311405,

        [EnumValue("2312908")]
        [Description("Sobral - CE")]
        Sobral = 2312908,

        [EnumValue("2313302")]
        [Description("Taua - CE")]
        Taua = 2313302,

        //Rio Grande do Norte
        [EnumValue("2403251")]
        [Description("Parnamirim - RN")]
        Parnamirim = 2403251,

        [EnumValue("2407104")]
        [Description("Macaiba - RN")]
        Macaiba = 2407104,

        [EnumValue("2408003")]
        [Description("Mossoro - RN")]
        Mossoro = 2408003,

        [EnumValue("2408102")]
        [Description("Natal - RN")]
        Natal = 2408102,

        [EnumValue("2411007")]
        [Description("Rodolfo Fernandes - RN")]
        RodolfoFernandes = 2411007,

        [EnumValue("2411056")]
        [Description("Tibau - RN")]
        Tibau = 2411056,

        [EnumValue("2412005")]
        [Description("Sao Goncalo do Amarante - RN")]
        SaoGoncalodoAmarante = 2412005,

        //Paraíba
        [EnumValue("2503209")]
        [Description("Cabedelo - PB")]
        Cabedelo = 2503209,

        [EnumValue("2504009")]
        [Description("Campina Grande - PB")]
        CampinaGrande = 2504009,

        [EnumValue("2504603")]
        [Description("Conde - PB")]
        Conde = 2504603,

        [EnumValue("2507507")]
        [Description("Joao Pessoa - PB")]
        JoaoPessoa = 2507507,

        [EnumValue("2511608")]
        [Description("Piloes - PB")]
        Piloes = 2511608,

        [EnumValue("2513653")]
        [Description("Santarem - PB")]
        SantaremPB = 2513653,

        //Pernambuco
        [EnumValue("2601904")]
        [Description("Bezerros - PE")]
        Bezerros = 2601904,

        [EnumValue("2602902")]
        [Description("Cabo de Santo Agostinho - PE")]
        CabodeSantoAgostinho = 2602902,

        [EnumValue("2603454")]
        [Description("Camaragibe - PE")]
        Camaragibe = 2603454,

        [EnumValue("2604106")]
        [Description("Caruaru - PE")]
        Caruaru = 2604106,

        [EnumValue("2606200")]
        [Description("Goiana - PE")]
        Goiana = 2606200,

        [EnumValue("2607901")]
        [Description("Jaboatao dos Guararapes - PE")]
        JaboataodosGuararapes = 2607901,

        [EnumValue("2609600")]
        [Description("Olinda - PE")]
        Olinda = 2609600,

        [EnumValue("2610707")]
        [Description("Paulista - PE")]
        Paulista = 2610707,

        [EnumValue("2611101")]
        [Description("Petrolina - PE")]
        Petrolina = 2611101,

        [EnumValue("2611606")]
        [Description("Recife - PE")]
        Recife = 2611606,

        [EnumValue("2616407")]
        [Description("Vitoria de Santo Antao - PE")]
        VitoriadeSantoAntao = 2616407,

        //Alagoas
        [EnumValue("2700300")]
        [Description("Arapiraca - AL")]
        Arapiraca = 2700300,

        [EnumValue("2702405")]
        [Description("Delmiro Gouveia - AL")]
        DelmiroGouveia = 2702405,

        [EnumValue("2704302")]
        [Description("Maceio - AL")]
        Maceio = 2704302,

        [EnumValue("2704708")]
        [Description("Marechal Deodoro - AL")]
        MarechalDeodoro = 2704708,

        [EnumValue("2708006")]
        [Description("Santana do Ipanema - AL")]
        SantanadoIpanema = 2708006,

        //Sergipe
        [EnumValue("2800308")]
        [Description("Aracaju - SE")]
        Aracaju = 2800308,

        [EnumValue("2800605")]
        [Description("Barra dos Coqueiros - SE")]
        BarradosCoqueiros = 2800605,

        [EnumValue("2801009")]
        [Description("Campo do Brito - SE")]
        CampodoBrito = 2801009,

        [EnumValue("2802106")]
        [Description("Estancia - SE")]
        Estancia = 2802106,

        [EnumValue("2802908")]
        [Description("Itabaiana - SE")]
        Itabaiana = 2802908,

        [EnumValue("2803302")]
        [Description("Japaratuba - SE")]
        Japaratuba = 2803302,

        [EnumValue("2803500")]
        [Description("Lagarto - SE")]
        Lagarto = 2803500,

        [EnumValue("2804805")]
        [Description("Nossa Senhora do Socorro - SE")]
        NossaSenhoradoSocorro = 2804805,

        //Bahia
        [EnumValue("2900801")]
        [Description("Alcobaca - BA")]
        Alcobaca = 2900801,

        [EnumValue("2901007")]
        [Description("Amargosa - BA")]
        Amargosa = 2901007,

        [EnumValue("2901106")]
        [Description("Amelia Rodrigues - BA")]
        AmeliaRodrigues = 2901106,

        [EnumValue("2902708")]
        [Description("Barra - BA")]
        Barra = 2902708,

        [EnumValue("2903201")]
        [Description("Barreiras - BA")]
        Barreiras = 2903201,

        [EnumValue("2903904")]
        [Description("Bom Jesus da Lapa - BA")]
        BomJesusdaLapa = 2903904,

        [EnumValue("2905602")]
        [Description("Camacan - BA")]
        Camacan = 2905602,

        [EnumValue("2905701")]
        [Description("Camacari - BA")]
        Camacari = 2905701,

        [EnumValue("2906006")]
        [Description("Campo Formoso - BA")]
        CampoFormoso = 2906006,

        [EnumValue("2906303")]
        [Description("Canavieiras - BA")]
        Canavieiras = 2906303,

        [EnumValue("2906501")]
        [Description("Candeias - BA")]
        Candeias = 2906501,

        [EnumValue("2907202")]
        [Description("Casa Nova - BA")]
        CasaNova = 2907202,

        [EnumValue("2907509")]
        [Description("Catu - BA")]
        Catu = 2907509,

        [EnumValue("2909307")]
        [Description("Correntina - BA")]
        Correntina = 2909307,

        [EnumValue("2909802")]
        [Description("Cruz das Almas - BA")]
        CruzdasAlmas = 2909802,

        [EnumValue("2910057")]
        [Description("Dias D Avila - BA")]
        DiasdAvila = 2910057,

        [EnumValue("2910503")]
        [Description("Entre Rios - BA")]
        EntreRios = 2910503,

        [EnumValue("2910602")]
        [Description("Esplanada - BA")]
        Esplanada = 2910602,

        [EnumValue("2910727")]
        [Description("Eunapolis - BA")]
        Eunapolis = 2910727,

        [EnumValue("2910800")]
        [Description("Feira de Santana - BA")]
        FeiradeSantana = 2910800,

        [EnumValue("2911709")]
        [Description("Guanambi - BA")]
        Guanambi = 2911709,

        [EnumValue("2913200")]
        [Description("Ibotirama - BA")]
        Ibotirama = 2913200,

        [EnumValue("2913606")]
        [Description("Ilheus - BA")]
        Ilheus = 2913606,

        [EnumValue("2913903")]
        [Description("Ipiau - BA")]
        Ipiau = 2913903,

        [EnumValue("2914505")]
        [Description("Irara - BA")]
        Irara = 2914505,

        [EnumValue("2914604")]
        [Description("Irece - BA")]
        Irece = 2914604,

        [EnumValue("2914653")]
        [Description("Itabela - BA")]
        Itabela = 2914653,

        [EnumValue("2914703")]
        [Description("Itaberaba - BA")]
        Itaberaba = 2914703,

        [EnumValue("2914802")]
        [Description("Itabuna - BA")]
        Itabuna = 2914802,

        [EnumValue("2916401")]
        [Description("Itapetinga - BA")]
        Itapetinga = 2916401,

        [EnumValue("2917003")]
        [Description("Itiuba - BA")]
        Itiuba = 2917003,

        [EnumValue("2917508")]
        [Description("Jacobina - BA")]
        Jacobina = 2917508,

        [EnumValue("2917706")]
        [Description("Jaguarari - BA")]
        Jaguarari = 2917706,

        [EnumValue("2918407")]
        [Description("Juazeiro - BA")]
        Juazeiro = 2918407,

        [EnumValue("2919157")]
        [Description("Lapao - BA")]
        Lapao = 2919157,

        [EnumValue("2919207")]
        [Description("Lauro de Freitas - BA")]
        LaurodeFreitas = 2919207,

        [EnumValue("2919553")]
        [Description("Luiz Eduardo Magalhaes - BA")]
        LuizEduardoMagalhaes = 2919553,

        [EnumValue("2919926")]
        [Description("Madre de Deus - BA")]
        MadredeDeus = 2919926,

        [EnumValue("2922003")]
        [Description("Mucuri - BA")]
        Mucuri = 2922003,

        [EnumValue("2922656")]
        [Description("Nordestina - BA")]
        Nordestina = 2922656,

        [EnumValue("2924801")]
        [Description("Piritiba - BA")]
        Piritiba = 2924801,

        [EnumValue("2925105")]
        [Description("Pocoes - BA")]
        Pocoes = 2925105,

        [EnumValue("2925204")]
        [Description("Pojuca - BA")]
        Pojuca = 2925204,

        [EnumValue("2925303")]
        [Description("Porto Seguro - BA")]
        PortoSeguro = 2925303,

        [EnumValue("2925501")]
        [Description("Prado - BA")]
        Prado = 2925501,

        [EnumValue("2927408")]
        [Description("Salvador - BA")]
        Salvador = 2927408,

        [EnumValue("2927705")]
        [Description("Santa Cruz Cabralia - BA")]
        SantaCruzCabralia = 2927705,

        [EnumValue("2928703")]
        [Description("Santo Antonio de Jesus - BA")]
        SantoAntoniodeJesus = 2928703,

        [EnumValue("2928901")]
        [Description("Sao Desiderio - BA")]
        SaoDesiderio = 2928901,

        [EnumValue("2929503")]
        [Description("Sao Sebastiao do Passe - BA")]
        SaoSebastiaodoPasse = 2929503,

        [EnumValue("2930105")]
        [Description("Senhor do Bonfim - BA")]
        SenhordoBonfim = 2930105,

        [EnumValue("2930709")]
        [Description("Simoes Filho - BA")]
        SimoesFilho = 2930709,

        [EnumValue("2931350")]
        [Description("Teixeira de Freitas - BA")]
        TeixeiradeFreitas = 2931350,

        [EnumValue("2932507")]
        [Description("Una - BA")]
        Una = 2932507,

        [EnumValue("2932606")]
        [Description("Urandi - BA")]
        Urandi = 2932606,

        [EnumValue("2932804")]
        [Description("Utinga - BA")]
        Utinga = 2932804,

        [EnumValue("2933208")]
        [Description("Vera Cruz - BA")]
        VeraCruzBA = 2933208,

        [EnumValue("2933307")]
        [Description("Vitoria da Conquista - BA")]
        VitoriadaConquista = 2933307,

        //Minas Gerais
        [EnumValue("3101409")]
        [Description("Albertina - MG")]
        Albertina = 3101409,

        [EnumValue("3101508")]
        [Description("Alem Paraiba - MG")]
        AlemParaiba = 3101508,

        [EnumValue("3101904")]
        [Description("Alpinopolis - MG")]
        Alpinopolis = 3101904,

        [EnumValue("3102605")]
        [Description("Andradas - MG")]
        Andradas = 3102605,

        [EnumValue("3102902")]
        [Description("Antonio Carlos - MG")]
        AntonioCarlosMG = 3102902,

        [EnumValue("3103405")]
        [Description("Aracuai - MG")]
        Aracuai = 3103405,

        [EnumValue("3103504")]
        [Description("Araguari - MG")]
        Araguari = 3103504,

        [EnumValue("3104007")]
        [Description("Araxa - MG")]
        Araxa = 3104007,

        [EnumValue("3104205")]
        [Description("Arcos - MG")]
        Arcos = 3104205,

        [EnumValue("3105608")]
        [Description("Barbacena - MG")]
        Barbacena = 3105608,

        [EnumValue("3105905")]
        [Description("Barroso - MG")]
        Barroso = 3105905,

        [EnumValue("3106200")]
        [Description("Belo Horizonte - MG")]
        BeloHorizonte = 3106200,

        [EnumValue("3106309")]
        [Description("Belo Oriente - MG")]
        BeloOriente = 3106309,

        [EnumValue("3106408")]
        [Description("Belo Vale - MG")]
        BeloVale = 3106408,

        [EnumValue("3106705")]
        [Description("Betim - MG")]
        Betim = 3106705,

        [EnumValue("3107109")]
        [Description("Boa Esperanca - MG")]
        BoaEsperanca = 3107109,

        [EnumValue("3107307")]
        [Description("Bocaiuva - MG")]
        Bocaiuva = 3107307,

        [EnumValue("3107406")]
        [Description("Bom Despacho - MG")]
        BomDespacho = 3107406,

        [EnumValue("3109006")]
        [Description("Brumadinho - MG")]
        Brumadinho = 3109006,

        [EnumValue("3110202")]
        [Description("Cajuri - MG")]
        Cajuri = 3110202,

        [EnumValue("3110400")]
        [Description("Camacho - MG")]
        Camacho = 3110400,

        [EnumValue("3110509")]
        [Description("Camanducaia - MG")]
        Camanducaia = 3110509,

        [EnumValue("3111002")]
        [Description("Campestre - MG")]
        Campestre = 3111002,

        [EnumValue("3111101")]
        [Description("Campina Verde - MG")]
        CampinaVerde = 3111101,

        [EnumValue("3111200")]
        [Description("Campo Belo - MG")]
        CampoBelo = 3111200,

        [EnumValue("3111507")]
        [Description("Campos Altos - MG")]
        CamposAltos = 3111507,

        [EnumValue("3112505")]
        [Description("Capim Branco - MG")]
        CapimBranco = 3112505,

        [EnumValue("3112604")]
        [Description("Capinopolis - MG")]
        Capinopolis = 3112604,

        [EnumValue("3113305")]
        [Description("Carangola - MG")]
        Carangola = 3113305,

        [EnumValue("3113404")]
        [Description("Caratinga - MG")]
        Caratinga = 3113404,

        [EnumValue("3114402")]
        [Description("Carmo do Rio Claro - MG")]
        CarmodoRioClaro = 3114402,

        [EnumValue("3114600")]
        [Description("Carrancas - MG")]
        Carrancas = 3114600,

        [EnumValue("3114907")]
        [Description("Casa Grande - MG")]
        CasaGrande = 3114907,

        [EnumValue("3115201")]
        [Description("Conceicao da Barra de Minas - MG")]
        ConceicaodaBarradeMinas = 3115201,

        [EnumValue("3115300")]
        [Description("Cataguases - MG")]
        Cataguases = 3115300,

        [EnumValue("3116605")]
        [Description("Claudio - MG")]
        Claudio = 3116605,

        [EnumValue("3116902")]
        [Description("Comendador Gomes - MG")]
        ComendadorGomes = 3116902,

        [EnumValue("3117504")]
        [Description("Conceicao do Mato Dentro - MG")]
        ConceicaodoMatoDentro = 3117504,

        [EnumValue("3117801")]
        [Description("Conceicao dos Ouros - MG")]
        ConceicaodosOuros = 3117801,

        [EnumValue("3117876")]
        [Description("Confins - MG")]
        Confins = 3117876,

        [EnumValue("3118304")]
        [Description("Conselheiro Lafaiete - MG")]
        ConselheiroLafaiete = 3118304,

        [EnumValue("3118601")]
        [Description("Contagem - MG")]
        Contagem = 3118601,

        [EnumValue("3119302")]
        [Description("Coromandel - MG")]
        Coromandel = 3119302,

        [EnumValue("3119401")]
        [Description("Coronel Fabriciano - MG")]
        CoronelFabriciano = 3119401,

        [EnumValue("3119708")]
        [Description("Coronel Xavier Chaves - MG")]
        CoronelXavierChaves = 3119708,

        [EnumValue("3120904")]
        [Description("Curvelo - MG")]
        Curvelo = 3120904,

        [EnumValue("3121209")]
        [Description("Delfinopolis - MG")]
        Delfinopolis = 3121209,

        [EnumValue("3121258")]
        [Description("Delta - MG")]
        Delta = 3121258,

        [EnumValue("3122306")]
        [Description("Divinopolis - MG")]
        Divinopolis = 3122306,

        [EnumValue("3123007")]
        [Description("Dores de Campos - MG")]
        DoresdeCampos = 3123007,

        [EnumValue("3123205")]
        [Description("Dores do Indaia - MG")]
        DoresdoIndaia = 3123205,

        [EnumValue("3123304")]
        [Description("Dores do Turvo - MG")]
        DoresdoTurvo = 3123304,

        [EnumValue("3123908")]
        [Description("Entre Rios de Minas - MG")]
        EntreRiosdeMinas = 3123908,

        [EnumValue("3124005")]
        [Description("Ervalia - MG")]
        Ervalia = 3124005,

        [EnumValue("3125101")]
        [Description("Extrema - MG")]
        Extrema = 3125101,

        [EnumValue("3126109")]
        [Description("Formiga - MG")]
        Formiga = 3126109,

        [EnumValue("3127008")]
        [Description("Fronteira - MG")]
        Fronteira = 3127008,

        [EnumValue("3127107")]
        [Description("Frutal - MG")]
        Frutal = 3127107,

        [EnumValue("3127206")]
        [Description("Funilandia - MG")]
        Funilandia = 3127206,

        [EnumValue("3127701")]
        [Description("Governador Valadares - MG")]
        GovernadorValadares = 3127701,

        [EnumValue("3128006")]
        [Description("Guanhaes - MG")]
        Guanhaes = 3128006,

        [EnumValue("3128105")]
        [Description("Guape - MG")]
        Guape = 3128105,

        [EnumValue("3128709")]
        [Description("Guaxupe - MG")]
        Guaxupe = 3128709,

        [EnumValue("3130002")]
        [Description("Ibituruna - MG")]
        Ibituruna = 3130002,

        [EnumValue("3130101")]
        [Description("Igarape - MG")]
        Igarape = 3130101,

        [EnumValue("3130200")]
        [Description("Igaratinga - MG")]
        Igaratinga = 3130200,

        [EnumValue("3130309")]
        [Description("Iguatama - MG")]
        Iguatama = 3130309,

        [EnumValue("3130804")]
        [Description("Ingai - MG")]
        Ingai = 3130804,

        [EnumValue("3131307")]
        [Description("Ipatinga - MG")]
        Ipatinga = 3131307,

        [EnumValue("3131703")]
        [Description("Itabira - MG")]
        Itabira = 3131703,

        [EnumValue("3132404")]
        [Description("Itajuba - MG")]
        Itajuba = 3132404,

        [EnumValue("3133303")]
        [Description("Itaobim - MG")]
        Itaobim = 3133303,

        [EnumValue("3133600")]
        [Description("Itapeva - MG")]
        ItapevaMG = 3133600,

        [EnumValue("3133709")]
        [Description("Itatiaiucu - MG")]
        Itatiaiucu = 3133709,

        [EnumValue("3133758")]
        [Description("Itau de Minas - MG")]
        ItaudeMinas = 3133758,

        [EnumValue("3133808")]
        [Description("Itauna - MG")]
        Itauna = 3133808,

        [EnumValue("3133907")]
        [Description("Itaverava - MG")]
        Itaverava = 3133907,

        [EnumValue("3134202")]
        [Description("Ituiutaba - MG")]
        Ituiutaba = 3134202,

        [EnumValue("3134301")]
        [Description("Itumirim - MG")]
        Itumirim = 3134301,

        [EnumValue("3134400")]
        [Description("Iturama - MG")]
        Iturama = 3134400,

        [EnumValue("3134509")]
        [Description("Itutinga - MG")]
        Itutinga = 3134509,

        [EnumValue("3134608")]
        [Description("Jaboticatubas - MG")]
        Jaboticatubas = 3134608,

        [EnumValue("3135050")]
        [Description("Jaiba - MG")]
        Jaiba = 3135050,

        [EnumValue("3135100")]
        [Description("Janauba - MG")]
        Janauba = 3135100,

        [EnumValue("3135209")]
        [Description("Januaria - MG")]
        Januaria = 3135209,

        [EnumValue("3135407")]
        [Description("Jeceaba - MG")]
        Jeceaba = 3135407,

        [EnumValue("3136207")]
        [Description("Joao Monlevade - MG")]
        JoaoMonlevade = 3136207,

        [EnumValue("3136702")]
        [Description("Juiz de Fora - MG")]
        JuizdeFora = 3136702,

        [EnumValue("3137205")]
        [Description("Lagoa da Prata - MG")]
        LagoadaPrata = 3137205,

        [EnumValue("3137403")]
        [Description("Lagoa Dourada - MG")]
        LagoaDourada = 3137403,

        [EnumValue("3137601")]
        [Description("Lagoa Santa - MG")]
        LagoaSantaMG = 3137601,

        [EnumValue("3138203")]
        [Description("Lavras - MG")]
        Lavras = 3138203,

        [EnumValue("3138401")]
        [Description("Leopoldina - MG")]
        Leopoldina = 3138401,

        [EnumValue("3138708")]
        [Description("Luminarias - MG")]
        Luminarias = 3138708,

        [EnumValue("3138807")]
        [Description("Luz - MG")]
        Luz = 3138807,

        [EnumValue("3139409")]
        [Description("Manhuacu - MG")]
        Manhuacu = 3139409,

        [EnumValue("3139508")]
        [Description("Manhumirim - MG")]
        Manhumirim = 3139508,

        [EnumValue("3139607")]
        [Description("Mantena - MG")]
        Mantena = 3139607,

        [EnumValue("3139805")]
        [Description("Mar de Espanha - MG")]
        MardeEspanha = 3139805,

        [EnumValue("3139904")]
        [Description("Maria da Fe - MG")]
        MariadaFe = 3139904,

        [EnumValue("3140001")]
        [Description("Mariana - MG")]
        Mariana = 3140001,

        [EnumValue("3140506")]
        [Description("Martinho Campos - MG")]
        MartinhoCampos = 3140506,

        [EnumValue("3140803")]
        [Description("Matias Barbosa - MG")]
        MatiasBarbosa = 3140803,

        [EnumValue("3141108")]
        [Description("Matozinhos - MG")]
        Matozinhos = 3141108,

        [EnumValue("3143104")]
        [Description("Monte Carmelo - MG")]
        MonteCarmelo = 3143104,

        [EnumValue("3143302")]
        [Description("Montes Claros - MG")]
        MontesClaros = 3143302,

        [EnumValue("3143401")]
        [Description("Monte Siao - MG")]
        MonteSiao = 3143401,

        [EnumValue("3143906")]
        [Description("Muriae - MG")]
        Muriae = 3143906,

        [EnumValue("3144508")]
        [Description("Nazareno - MG")]
        Nazareno = 3144508,

        [EnumValue("3144607")]
        [Description("Nepomuceno - MG")]
        Nepomuceno = 3144607,

        [EnumValue("3144805")]
        [Description("Nova Lima - MG")]
        NovaLima = 3144805,

        [EnumValue("3145208")]
        [Description("Nova Serrana - MG")]
        NovaSerrana = 3145208,

        [EnumValue("3146008")]
        [Description("Ouro Fino - MG")]
        OuroFino = 3146008,

        [EnumValue("3146107")]
        [Description("Ouro Preto - MG")]
        OutroPreto = 3146107,

        [EnumValue("3146305")]
        [Description("Padre Paraiso - MG")]
        PadreParaiso = 3146305,

        [EnumValue("3147006")]
        [Description("Paracatu - MG")]
        Paracatu = 3147006,

        [EnumValue("3147105")]
        [Description("Para de Minas - MG")]
        ParadeMinas = 3147105,

        [EnumValue("3147907")]
        [Description("Passos - MG")]
        Passos = 3147907,

        [EnumValue("3148004")]
        [Description("Patos de Minas - MG")]
        PatosdeMinas = 3148004,

        [EnumValue("3148103")]
        [Description("Patrocinio - MG")]
        Patrocinio = 3148103,

        [EnumValue("3149309")]
        [Description("Pedro Leopoldo - MG")]
        PedroLeopoldo = 3149309,

        [EnumValue("3149606")]
        [Description("Pequi - MG")]
        Pequi = 3149606,

        [EnumValue("3149903")]
        [Description("Perdoes - MG")]
        Perdoes = 3149903,

        [EnumValue("3150703")]
        [Description("Pirajuba - MG")]
        Pirajuba = 3150703,

        [EnumValue("3151107")]
        [Description("Pirapetinga - MG")]
        Pirapetinga = 3151107,

        [EnumValue("3151206")]
        [Description("Pirapora - MG")]
        Pirapora = 3151206,

        [EnumValue("3151503")]
        [Description("Piumhi - MG")]
        Piumhi = 3151503,

        [EnumValue("3151602")]
        [Description("Planura - MG")]
        Planura = 3151602,

        [EnumValue("3151701")]
        [Description("Poco Fundo - MG")]
        PocoFundo = 3151701,

        [EnumValue("3151800")]
        [Description("Pocos de Caldas - MG")]
        PocosdeCaldas = 3151800,

        [EnumValue("3152006")]
        [Description("Pompeu - MG")]
        Pompeu = 3152006,

        [EnumValue("3152105")]
        [Description("Ponte Nova - MG")]
        PonteNova = 3152105,

        [EnumValue("3152204")]
        [Description("Porteirinha - MG")]
        Porteirinha = 3152204,

        [EnumValue("3152303")]
        [Description("Porto Firme - MG")]
        PortoFirme = 3152303,

        [EnumValue("3152501")]
        [Description("Pouso Alegre - MG")]
        PousoAlegre = 3152501,

        [EnumValue("3152600")]
        [Description("Pouso Alto - MG")]
        PousoAlto = 3152600,

        [EnumValue("3152709")]
        [Description("Prados - MG")]
        Prados = 3152709,

        [EnumValue("3152808")]
        [Description("Prata - MG")]
        Prata = 3152808,

        [EnumValue("3153608")]
        [Description("Prudente de Morais - MG")]
        PrudentedeMorais = 3153608,

        [EnumValue("3153905")]
        [Description("Raposos - MG")]
        Raposos = 3153905,

        [EnumValue("3154200")]
        [Description("Resende Costa - MG")]
        ResendeCosta = 3154200,

        [EnumValue("3154606")]
        [Description("Ribeirao das Neves - MG")]
        RibeiraodasNeves = 3154606,

        [EnumValue("3154804")]
        [Description("Rio Acima - MG")]
        RioAcima = 3154804,

        [EnumValue("3155702")]
        [Description("Rio Piracicaba - MG")]
        RioPiracicaba = 3155702,

        [EnumValue("3156106")]
        [Description("Ritapolis - MG")]
        Ritapolis = 3156106,

        [EnumValue("3156700")]
        [Description("Sabara - MG")]
        Sabara = 3156700,

        [EnumValue("3156908")]
        [Description("Sacramento - MG")]
        Sacramento = 3156908,

        [EnumValue("3157203")]
        [Description("Santa Barbara - MG")]
        SantaBarbara = 3157203,

        [EnumValue("3157302")]
        [Description("Santa Barbara do Tugurio - MG")]
        SantaBarbaradoTugurio = 3157302,

        [EnumValue("3157336")]
        [Description("Santa Cruz de Minas - MG")]
        SantaCruzdeMinas = 3157336,

        [EnumValue("3157807")]
        [Description("Santa Luzia - MG")]
        SantaLuzia = 3157807,

        [EnumValue("3158805")]
        [Description("Santana do Jacare - MG")]
        SantanadoJacare = 3158805,

        [EnumValue("3159001")]
        [Description("Santana do Riacho - MG")]
        SantanadoRiacho = 3159001,

        [EnumValue("3159100")]
        [Description("Santana dos Montes - MG")]
        SantanadosMontes = 3159100,

        [EnumValue("3159605")]
        [Description("Santa Rita do Sapucai - MG")]
        SantaRitadoSapucai = 3159605,

        [EnumValue("3159803")]
        [Description("Santa Vitoria - MG")]
        SantaVitoria = 3159803,

        [EnumValue("3159902")]
        [Description("Santo Antonio do Amparo - MG")]
        SantoAntoniodoAmparo = 3159902,

        [EnumValue("3160405")]
        [Description("Santo Antonio do Monte - MG")]
        SantoAntoniodoMonte = 3160405,

        [EnumValue("3160702")]
        [Description("Santos Dumont - MG")]
        SantosDumont = 3160702,

        [EnumValue("3160900")]
        [Description("Sao Bras do Suacui - MG")]
        SaoBrasdoSuacui = 3160900,

        [EnumValue("3160959")]
        [Description("Sao Domingos das Dores - MG")]
        SaoDomingosdasDores = 3160959,

        [EnumValue("3161106")]
        [Description("Sao Francisco - MG")]
        SaoFrancisco = 3161106,

        [EnumValue("3161502")]
        [Description("Sao Geraldo - MG")]
        SaoGeraldo = 3161502,

        [EnumValue("3161809")]
        [Description("Sao Goncalo do Para - MG")]
        SaoGoncalodoPara = 3161809,

        [EnumValue("3162104")]
        [Description("Sao Gotardo - MG")]
        SaoGotardo = 3162104,

        [EnumValue("3162500")]
        [Description("Sao Joao Del Rei - MG")]
        SaoJoaoDelRei = 3162500,

        [EnumValue("3162922")]
        [Description("Sao Joaquim de Bicas - MG")]
        SaoJoaquimdeBicas = 3162922,

        [EnumValue("3162955")]
        [Description("Sao Jose da Lapa - MG")]
        SaoJosedaLapa = 3162955,

        [EnumValue("3163706")]
        [Description("Sao Lourenco - MG")]
        SaoLourenco = 3163706,

        [EnumValue("3164704")]
        [Description("Sao Sebastiao do Paraiso - MG")]
        SaoSebastiaodoParaiso = 3164704,

        [EnumValue("3165008")]
        [Description("Sao Tiago - MG")]
        SaoTiago = 3165008,

        [EnumValue("3165305")]
        [Description("Sao Vicente de Minas - MG")]
        SaoVicentedeMinas = 3165305,

        [EnumValue("3165701")]
        [Description("Senador Firmino - MG")]
        SenadorFirmino = 3165701,

        [EnumValue("3167202")]
        [Description("Sete Lagoas - MG")]
        SeteLagoas = 3167202,

        [EnumValue("3167301")]
        [Description("Silveirania - MG")]
        Silveirania = 3167301,

        [EnumValue("3168606")]
        [Description("Teofilo Otoni - MG")]
        TeofiloOtoni = 3168606,

        [EnumValue("3168804")]
        [Description("Tiradentes - MG")]
        Tiradentes = 3168804,

        [EnumValue("3169307")]
        [Description("Tres Coracoes - MG")]
        TresCoracoes = 3169307,

        [EnumValue("3169356")]
        [Description("Tres Marias - MG")]
        TresMarias = 3169356,

        [EnumValue("3169505")]
        [Description("Tumiritinga - MG")]
        Tumiritinga = 3169505,

        [EnumValue("3169901")]
        [Description("Uba - MG")]
        Uba = 3169901,

        [EnumValue("3170107")]
        [Description("Uberaba - MG")]
        Uberaba = 3170107,

        [EnumValue("3170206")]
        [Description("Uberlandia - MG")]
        Uberlandia = 3170206,

        [EnumValue("3170404")]
        [Description("Unai - MG")]
        Unai = 3170404,

        [EnumValue("3170701")]
        [Description("Varginha - MG")]
        Varginha = 3170701,

        [EnumValue("3170800")]
        [Description("Varzea da Palma - MG")]
        VarzeadaPalma = 3170800,

        [EnumValue("3171006")]
        [Description("Vazante - MG")]
        Vazante = 3171006,

        [EnumValue("3171105")]
        [Description("Verissimo - MG")]
        Verissimo = 3171105,

        [EnumValue("3171204")]
        [Description("Vespasiano - MG")]
        Vespasiano = 3171204,

        [EnumValue("3171303")]
        [Description("Vicosa - MG")]
        Vicosa = 3171303,

        [EnumValue("3172004")]
        [Description("Visconde do Rio Branco - MG")]
        ViscondedoRioBranco = 3172004,

        //Espírito Santo
        [EnumValue("3200102")]
        [Description("Afonso Claudio - ES")]
        AfonsoClaudio = 3200102,

        [EnumValue("3200136")]
        [Description("Aguia Branca - ES")]
        AguiaBranca = 3200136,

        [EnumValue("3200300")]
        [Description("Alfredo Chaves - ES")]
        AlfredoChaves = 3200300,

        [EnumValue("3200409")]
        [Description("Anchieta - ES")]
        AnchietaES = 3200409,

        [EnumValue("3200607")]
        [Description("Aracruz - ES")]
        Aracruz = 3200607,

        [EnumValue("3200706")]
        [Description("Atilio Vivacqua - ES")]
        AtilioVivacqua = 3200706,

        [EnumValue("3201209")]
        [Description("Cachoeiro do Itapemirim - ES")]
        CachoeirodoItapemirim = 3201209,

        [EnumValue("3201308")]
        [Description("Cariacica - ES")]
        Cariacica = 3201308,

        [EnumValue("3201506")]
        [Description("Colatina - ES")]
        Colatina = 3201506,

        [EnumValue("3201902")]
        [Description("Domingos Martins - ES")]
        DomingosMartins = 3201902,

        [EnumValue("3202405")]
        [Description("Guarapari - ES")]
        Guarapari = 3202405,

        [EnumValue("3202454")]
        [Description("Ibatiba - ES")]
        Ibatiba = 3202454,

        [EnumValue("3202504")]
        [Description("Ibiracu - ES")]
        Ibiracu = 3202504,

        [EnumValue("3202553")]
        [Description("Ibitirama - ES")]
        Ibitirama = 3202553,

        [EnumValue("3202603")]
        [Description("Iconha - ES")]
        Iconha = 3202603,

        [EnumValue("3203007")]
        [Description("Iuna - ES")]
        Iuna = 3203007,

        [EnumValue("3203205")]
        [Description("Linhares - ES")]
        Linhares = 3203205,

        [EnumValue("3203320")]
        [Description("Marataizes - ES")]
        Marataizes = 3203320,

        [EnumValue("3203346")]
        [Description("Marechal Floriano - ES")]
        MarechalFloriano = 3203346,

        [EnumValue("3203908")]
        [Description("Nova Venecia - ES")]
        NovaVenecia = 3203908,

        [EnumValue("3204104")]
        [Description("Pinheiros - ES")]
        Pinheiros = 3204104,

        [EnumValue("3204559")]
        [Description("Santa Maria de Jetiba - ES")]
        SantaMariadeJetiba = 3204559,

        [EnumValue("3204708")]
        [Description("Sao Gabriel da Palha - ES")]
        SaoGabrieldaPalha = 3204708,

        [EnumValue("3204906")]
        [Description("Sao Mateus - ES")]
        SaoMateus = 3204906,

        [EnumValue("3205002")]
        [Description("Serra - ES")]
        Serra = 3205002,

        [EnumValue("3205036")]
        [Description("Vargem Alta - ES")]
        VargemAlta = 3205036,

        [EnumValue("3205069")]
        [Description("Venda Nova do Imigrante - ES")]
        VendaNovadoImigrante = 3205069,

        [EnumValue("3205101")]
        [Description("Viana - ES")]
        Viana = 3205101,

        [EnumValue("3205200")]
        [Description("Vila Velha - ES")]
        VilaVelha = 3205200,

        [EnumValue("3205309")]
        [Description("Vitoria - ES")]
        Vitoria = 3205309,

        //Rio de Janeiro
        [EnumValue("3300100")]
        [Description("Angra dos Reis - RJ")]
        AngradosReis = 3300100,

        [EnumValue("3300209")]
        [Description("Araruama - RJ")]
        Araruama = 3300209,

        [EnumValue("3300225")]
        [Description("Areal - RJ")]
        Areal = 3300225,

        [EnumValue("3300233")]
        [Description("Armacao dos Buzios - RJ")]
        ArmacaodosBuzios = 3300233,

        [EnumValue("3300407")]
        [Description("Barra Mansa - RJ")]
        BarraMansa = 3300407,

        [EnumValue("3300456")]
        [Description("Belford Roxo - RJ")]
        BelfordRoxo = 3300456,

        [EnumValue("3300704")]
        [Description("Cabo Frio - RJ")]
        CaboFrio = 3300704,

        [EnumValue("3300803")]
        [Description("Cachoeiras de Macacu - RJ")]
        CachoeirasdeMacacu = 3300803,

        [EnumValue("3300936")]
        [Description("Carapebus - RJ")]
        Carapebus = 3300936,

        [EnumValue("3300951")]
        [Description("Comendador Levy Gasparian - RJ")]
        ComendadorLevyGasparian = 3300951,

        [EnumValue("3301009")]
        [Description("Campos Dos Goytacazes - RJ")]
        CamposDosGoytacazes = 3301009,

        [EnumValue("3301207")]
        [Description("Carmo - RJ")]
        Carmo = 3301207,

        [EnumValue("3301306")]
        [Description("Casimiro de Abreu - RJ")]
        CasimirodeAbreu = 3301306,

        [EnumValue("3301405")]
        [Description("Conceicao de Macabu - RJ")]
        ConceicaodeMacabu = 3301405,

        [EnumValue("3301504")]
        [Description("Cordeiro - RJ")]
        Cordeiro = 3301504,

        [EnumValue("3301702")]
        [Description("Duque de Caxias - RJ")]
        DuquedeCaxias = 3301702,

        [EnumValue("3301801")]
        [Description("Engenheiro Paulo de Frontin - RJ")]
        EngenheiroPaulodeFrontin = 3301801,

        [EnumValue("3301876")]
        [Description("Iguaba Grande - RJ")]
        IguabaGrande = 3301876,

        [EnumValue("3301900")]
        [Description("Itaborai - RJ")]
        Itaborai = 3301900,

        [EnumValue("3302007")]
        [Description("Itaguai - RJ")]
        Itaguai = 3302007,

        [EnumValue("3302205")]
        [Description("Itaperuna - RJ")]
        Itaperuna = 3302205,

        [EnumValue("3302254")]
        [Description("Itatiaia - RJ")]
        Itatiaia = 3302254,

        [EnumValue("3302403")]
        [Description("Macae - RJ")]
        Macae = 3302403,

        [EnumValue("3302502")]
        [Description("Mage - RJ")]
        Mage = 3302502,

        [EnumValue("3302601")]
        [Description("Mangaratiba - RJ")]
        Mangaratiba = 3302601,

        [EnumValue("3302700")]
        [Description("Marica - RJ")]
        Marica = 3302700,

        [EnumValue("3302809")]
        [Description("Mendes - RJ")]
        Mendes = 3302809,

        [EnumValue("3302858")]
        [Description("Mesquita - RJ")]
        Mesquita = 3302858,

        [EnumValue("3302908")]
        [Description("Miguel Pereira - RJ")]
        MiguelPereira = 3302908,

        [EnumValue("3303302")]
        [Description("Niteroi - RJ")]
        Niteroi = 3303302,

        [EnumValue("3303401")]
        [Description("Nova Friburgo - RJ")]
        NovaFriburgo = 3303401,

        [EnumValue("3303500")]
        [Description("Nova Iguacu - RJ")]
        NovaIguacu = 3303500,

        [EnumValue("3303609")]
        [Description("Paracambi - RJ")]
        Paracambi = 3303609,

        [EnumValue("3303807")]
        [Description("Parati - RJ")]
        Parati = 3303807,

        [EnumValue("3303856")]
        [Description("Paty do Alferes - RJ")]
        PatydoAlferes = 3303856,

        [EnumValue("3303906")]
        [Description("Petropolis - RJ")]
        Petropolis = 3303906,

        [EnumValue("3303955")]
        [Description("Pinheiral - RJ")]
        Pinheiral = 3303955,

        [EnumValue("3304003")]
        [Description("Piraí - RJ")]
        Pirai = 3304003,

        [EnumValue("3304110")]
        [Description("Porto Real - RJ")]
        PortoReal = 3304110,

        [EnumValue("3304128")]
        [Description("Quatis - RJ")]
        Quatis = 3304128,

        [EnumValue("3304201")]
        [Description("Resende - RJ")]
        Resende = 3304201,

        [EnumValue("3304300")]
        [Description("Rio Bonito - RJ")]
        RioBonito = 3304300,

        [EnumValue("3304508")]
        [Description("Rio das Flores - RJ")]
        RiodasFlores = 3304508,

        [EnumValue("3304524")]
        [Description("Rio das Ostras - RJ")]
        RiodasOstras = 3304524,

        [EnumValue("3304557")]
        [Description("Rio de Janeiro - RJ")]
        RiodeJaneiro = 3304557,

        [EnumValue("3304607")]
        [Description("Santa Maria Madalena - RJ")]
        SantaMariaMadalena = 3304607,

        [EnumValue("3304706")]
        [Description("Santo Antonio de Padua - RJ")]
        SantoAntoniodePadua = 3304706,

        [EnumValue("3304904")]
        [Description("Sao Goncalo - RJ")]
        SaoGoncalo = 3304904,

        [EnumValue("3305000")]
        [Description("Sao Joao da Barra - RJ")]
        SaoJoaodaBarra = 3305000,

        [EnumValue("3305158")]
        [Description("Sao Joao de Meriti - RJ")]
        SaoJoaodeMeriti = 3305109,

        [EnumValue("3305158")]
        [Description("Sao Jose do Vale do Rio Preto - RJ")]
        SaoJosedoValedoRioPreto = 3305109,

        [EnumValue("3305208")]
        [Description("Sao Pedro da Aldeia - RJ")]
        SaoPedrodaAldeia = 3305208,

        [EnumValue("3305505")]
        [Description("Saquarema - RJ")]
        Saquarema = 3305505,

        [EnumValue("3305604")]
        [Description("Silva Jardim - RJ")]
        SilvaJardim = 3305604,

        [EnumValue("3305802")]
        [Description("Teresopolis - RJ")]
        Teresopolis = 3305802,

        [EnumValue("3305901")]
        [Description("Trajano de Morais - RJ")]
        TrajanodeMorais = 3305901,

        [EnumValue("3306008")]
        [Description("Tres Rios - RJ")]
        TresRios = 3306008,

        [EnumValue("3306107")]
        [Description("Valenca - RJ")]
        Valenca = 3306107,

        [EnumValue("3306206")]
        [Description("Vassouras - RJ")]
        Vassouras = 3306206,

        [EnumValue("3306206")]
        [Description("Volta Redonda - RJ")]
        VoltaRedonda = 3306206,

        //São Paulo
        [EnumValue("3500105")]
        [Description("Adamantina - SP")]
        Adamantina = 3500105,

        [EnumValue("3500303")]
        [Description("Aguai - SP")]
        Aguai = 3500303,

        [EnumValue("3500709")]
        [Description("Agudos - SP")]
        Agudos = 3500709,

        [EnumValue("3501608")]
        [Description("Americana - SP")]
        Americana = 3501608,

        [EnumValue("3501806")]
        [Description("Americo de Campos - SP")]
        AmericodeCampos = 3501806,

        [EnumValue("3501905")]
        [Description("Amparo - SP")]
        Amparo = 3501905,

        [EnumValue("3502002")]
        [Description("Analandia - SP")]
        Analandia = 3502002,

        [EnumValue("3502101")]
        [Description("Andradina - SP")]
        Andradina = 3502101,

        [EnumValue("3502200")]
        [Description("Angatuba - SP")]
        Angatuba = 3502200,

        [EnumValue("3502507")]
        [Description("Aparecida - SP")]
        Aparecida = 3502507,

        [EnumValue("3502804")]
        [Description("Aracatuba - SP")]
        Aracatuba = 3502804,

        [EnumValue("3502903")]
        [Description("Aracoiaba da Serra - SP")]
        AracoiabadaSerra = 3502903,

        [EnumValue("3503208")]
        [Description("Araraquara - SP")]
        Araraquara = 3503208,

        [EnumValue("3503307")]
        [Description("Araras - SP")]
        Araras = 3503307,

        [EnumValue("3503703")]
        [Description("Ariranha - SP")]
        Ariranha = 3503703,

        [EnumValue("3503901")]
        [Description("Aruja - SP")]
        Aruja = 3503901,

        [EnumValue("3504008")]
        [Description("Assis - SP")]
        Assis = 3504008,

        [EnumValue("3504107")]
        [Description("Atibaia - SP")]
        Atibaia = 3504107,

        [EnumValue("3504503")]
        [Description("Avare - SP")]
        Avare = 3504503,

        [EnumValue("3504602")]
        [Description("Bady Bassitt - SP")]
        BadyBassitt = 3504602,

        [EnumValue("3504800")]
        [Description("Balsamo - SP")]
        Balsamo = 3504800,

        [EnumValue("3505203")]
        [Description("Bariri - SP")]
        Bariri = 3505203,

        [EnumValue("3505500")]
        [Description("Barretos - SP")]
        Barretos = 3505500,

        [EnumValue("3505609")]
        [Description("Barrinha - SP")]
        Barrinha = 3505609,

        [EnumValue("3505708")]
        [Description("Barueri - SP")]
        Barueri = 3505708,

        [EnumValue("3505906")]
        [Description("Batatais - SP")]
        Batatais = 3505906,

        [EnumValue("3506003")]
        [Description("Bauru - SP")]
        Bauru = 3506003,

        [EnumValue("3506102")]
        [Description("Bebedouro - SP")]
        Bebedouro = 3506102,

        [EnumValue("3506359")]
        [Description("Bertioga - SP")]
        Bertioga = 3506359,

        [EnumValue("3506508")]
        [Description("Birigui - SP")]
        Birigui = 3506508,

        [EnumValue("3506904")]
        [Description("Bofete - SP")]
        Bofete = 3506904,

        [EnumValue("3507001")]
        [Description("Boituva - SP")]
        Boituva = 3507001,

        [EnumValue("3507506")]
        [Description("Botucatu - SP")]
        Botucatu = 3507506,

        [EnumValue("3507605")]
        [Description("Braganca Paulista - SP")]
        BragancaPaulista = 3507605,

        [EnumValue("3507803")]
        [Description("Brodowski - SP")]
        Brodowski = 3507803,

        [EnumValue("3507902")]
        [Description("Brotas - SP")]
        Brotas = 3507902,

        [EnumValue("3508108")]
        [Description("Buritama - SP")]
        Buritama = 3508108,

        [EnumValue("3508405")]
        [Description("Cabreuva - SP")]
        Cabreuva = 3508405,

        [EnumValue("3508603")]
        [Description("Cachoeira Paulista - SP")]
        CachoeiraPaulista = 3508603,

        [EnumValue("3509007")]
        [Description("Caieiras - SP")]
        Caieiras = 3509007,

        [EnumValue("3509205")]
        [Description("Cajamar - SP")]
        Cajamar = 3509205,

        [EnumValue("3509502")]
        [Description("Campinas - SP")]
        Campinas = 3509502,

        [EnumValue("3509601")]
        [Description("Campo Limpo Paulista - SP")]
        CampoLimpoPaulista = 3509601,

        [EnumValue("3510104")]
        [Description("Candido Rodrigues - SP")]
        CandidoRodrigues = 3510104,

        [EnumValue("3510203")]
        [Description("Capao Bonito - SP")]
        CapaoBonito = 3510203,

        [EnumValue("3510302")]
        [Description("Capela do Alto - SP")]
        CapeladoAlto = 3510302,

        [EnumValue("3510401")]
        [Description("Capivari - SP")]
        Capivari = 3510401,

        [EnumValue("3510500")]
        [Description("Caraguatatuba - SP")]
        Caraguatatuba = 3510500,

        [EnumValue("3510609")]
        [Description("Carapicuiba - SP")]
        Carapicuiba = 3510609,

        [EnumValue("3510807")]
        [Description("Casa Branca - SP")]
        CasaBranca = 3510807,

        [EnumValue("3511102")]
        [Description("Catanduva - SP")]
        Catanduva = 3511102,

        [EnumValue("3511300")]
        [Description("Cedral - SP")]
        Cedral = 3511300,

        [EnumValue("3511508")]
        [Description("Cerquilho - SP")]
        Cerquilho = 3511508,

        [EnumValue("3512001")]
        [Description("Colina - SP")]
        Colina = 3512001,

        [EnumValue("3512209")]
        [Description("Conchal - SP")]
        Conchal = 3512209,

        [EnumValue("3512407")]
        [Description("Cordeiropolis - SP")]
        Cordeiropolis = 3512407,

        [EnumValue("3512506")]
        [Description("Coroados - SP")]
        Coroados = 3512506,

        [EnumValue("3512704")]
        [Description("Corumbatai - SP")]
        Corumbatai = 3512704,

        [EnumValue("3512902")]
        [Description("Cosmorama - SP")]
        Cosmorama = 3512902,

        [EnumValue("3513009")]
        [Description("Cotia - SP")]
        Cotia = 3513009,

        [EnumValue("3513405")]
        [Description("Cruzeiro - SP")]
        Cruzeiro = 3513405,

        [EnumValue("3513504")]
        [Description("Cubatao - SP")]
        Cubatao = 3513504,

        [EnumValue("3513603")]
        [Description("Cunha - SP")]
        Cunha = 3513603,

        [EnumValue("3513801")]
        [Description("Diadema - SP")]
        Diadema = 3513801,

        [EnumValue("3514106")]
        [Description("Dois Corregos - SP")]
        DoisCorregos = 3514106,

        [EnumValue("3514304")]
        [Description("Dourado - SP")]
        Dourado = 3514304,

        [EnumValue("3514403")]
        [Description("Dracena - SP")]
        Dracena = 3514403,

        [EnumValue("3514502")]
        [Description("Duartina - SP")]
        Duartina = 3514502,

        [EnumValue("3515004")]
        [Description("Embu das Artes - SP")]
        EmbudasArtes = 3515004,

        [EnumValue("3515103")]
        [Description("Embu-Guacu - SP")]
        EmbuGuacu = 3515103,

        [EnumValue("3515152")]
        [Description("Engenheiro Coelho - SP")]
        EngenheiroCoelho = 3515152,

        [EnumValue("3515186")]
        [Description("Espirito Santo do Pinhal - SP")]
        EspiritoSantodoPinhal = 3515186,

        [EnumValue("3515509")]
        [Description("Fernandopolis - SP")]
        Fernandopolis = 3515509,

        [EnumValue("3515707")]
        [Description("Ferraz de Vasconcelos - SP")]
        FerrazdeVasconcelos = 3515707,

        [EnumValue("3516200")]
        [Description("Franca - SP")]
        Franca = 3516200,

        [EnumValue("3516705")]
        [Description("Garca - SP")]
        Garca = 3516705,

        [EnumValue("3516853")]
        [Description("Gaviao Peixoto - SP")]
        GaviaoPeixoto = 3516853,

        [EnumValue("3517208")]
        [Description("Guaicara - SP")]
        Guaicara = 3517208,

        [EnumValue("3517406")]
        [Description("Guaira - SP")]
        GuairaSP = 3517406,

        [EnumValue("3517505")]
        [Description("Guapiaçu - SP")]
        Guapiacu = 3517505,

        [EnumValue("3518206")]
        [Description("Guararapes - SP")]
        Guararapes = 3518206,

        [EnumValue("3518305")]
        [Description("Guararema - SP")]
        Guararema = 3518305,

        [EnumValue("3518404")]
        [Description("Guaratingueta - SP")]
        Guaratingueta = 3518404,

        [EnumValue("3518701")]
        [Description("Guaruja - SP")]
        Guaruja = 3518701,

        [EnumValue("3518800")]
        [Description("Guarulhos - SP")]
        Guarulhos = 3518800,

        [EnumValue("3518859")]
        [Description("Guatapara - SP")]
        Guatapara = 3518859,

        [EnumValue("3519006")]
        [Description("Herculandia - SP")]
        Herculandia = 3519006,

        [EnumValue("3519071")]
        [Description("Hortolandia - SP")]
        Hortolandia = 3519071,

        [EnumValue("3519105")]
        [Description("Iacanga - SP")]
        Iacanga = 3519105,

        [EnumValue("3519303")]
        [Description("Ibate - SP")]
        Ibate = 3519303,

        [EnumValue("3519600")]
        [Description("Ibitinga - SP")]
        Ibitinga = 3519600,

        [EnumValue("3519709")]
        [Description("Ibiuna - SP")]
        Ibiuna = 3519709,

        [EnumValue("3520004")]
        [Description("Igaracu do Tiete - SP")]
        IgaracudoTiete = 3520004,

        [EnumValue("3520103")]
        [Description("Igarapava - SP")]
        Igarapava = 3520103,

        [EnumValue("3520509")]
        [Description("Indaiatuba - SP")]
        Indaiatuba = 3520509,

        [EnumValue("3520608")]
        [Description("Indiana - SP")]
        Indiana = 3520608,

        [EnumValue("3521408")]
        [Description("Iracemapolis - SP")]
        Iracemapolis = 3521408,

        [EnumValue("3521903")]
        [Description("Itajobi - SP")]
        Itajobi = 3521903,

        [EnumValue("3522109")]
        [Description("Itanhaem - SP")]
        Itanhaem = 3522109,

        [EnumValue("3522307")]
        [Description("Itapetininga - SP")]
        Itapetininga = 3522307,

        [EnumValue("3522406")]
        [Description("Itapeva - SP")]
        ItapevaSP = 3522406,

        [EnumValue("3522505")]
        [Description("Itapevi - SP")]
        Itapevi = 3522505,

        [EnumValue("3522604")]
        [Description("Itapira - SP")]
        Itapira = 3522604,

        [EnumValue("3522703")]
        [Description("Itapolis - SP")]
        Itapolis = 3522703,

        [EnumValue("3523107")]
        [Description("Itaquaquecetuba - SP")]
        Itaquaquecetuba = 3523107,

        [EnumValue("3523404")]
        [Description("Itatiba - SP")]
        Itatiba = 3523404,

        [EnumValue("3523503")]
        [Description("Itatinga - SP")]
        Itatinga = 3523503,

        [EnumValue("3523909")]
        [Description("Itu - SP")]
        Itu = 3523909,

        [EnumValue("3524006")]
        [Description("Itupeva - SP")]
        Itupeva = 3524006,

        [EnumValue("3524303")]
        [Description("Jaboticabal - SP")]
        Jaboticabal = 3524303,

        [EnumValue("3524402")]
        [Description("Jacarei - SP")]
        Jacarei = 3524402,

        [EnumValue("3524501")]
        [Description("Jaci - SP")]
        Jaci = 3524501,

        [EnumValue("3524709")]
        [Description("Jaguariuna - SP")]
        Jaguariuna = 3524709,

        [EnumValue("3524808")]
        [Description("Jales - SP")]
        Jales = 3524808,

        [EnumValue("3524907")]
        [Description("Jambeiro - SP")]
        Jambeiro = 3524907,

        [EnumValue("3525003")]
        [Description("Jandira - SP")]
        Jandira = 3525003,

        [EnumValue("3525102")]
        [Description("Jardinopolis - SP")]
        JardinopolisSP = 3525102,

        [EnumValue("3525300")]
        [Description("Jau - SP")]
        Jau = 3525300,

        [EnumValue("3525508")]
        [Description("Joanopolis - SP")]
        Joanopolis = 3525508,

        [EnumValue("3525904")]
        [Description("Jundiai - SP")]
        Jundiai = 3525904,

        [EnumValue("3526001")]
        [Description("Junqueiropolis - SP")]
        Junqueiropolis = 3526001,

        [EnumValue("3526407")]
        [Description("Laranjal Paulista - SP")]
        LaranjalPaulista = 3526407,

        [EnumValue("3526704")]
        [Description("Leme - SP")]
        Leme = 3526704,

        [EnumValue("3526803")]
        [Description("Lencois Paulista - SP")]
        LencoisPaulista = 3526803,

        [EnumValue("3526902")]
        [Description("Limeira - SP")]
        Limeira = 3526902,

        [EnumValue("3527207")]
        [Description("Lorena - SP")]
        Lorena = 3527207,

        [EnumValue("3527405")]
        [Description("Lucelia - SP")]
        Lucelia = 3527405,

        [EnumValue("3527603")]
        [Description("Luis Antonio - SP")]
        LuisAntonio = 3527603,

        [EnumValue("3528007")]
        [Description("Macatuba - SP")]
        Macatuba = 3528007,

        [EnumValue("3528502")]
        [Description("Mairipora - SP")]
        Mairipora = 3528502,

        [EnumValue("3529005")]
        [Description("Marilia - SP")]
        Marilia = 3529005,

        [EnumValue("3529302")]
        [Description("Matao - SP")]
        Matao = 3529302,

        [EnumValue("3529401")]
        [Description("Maua - SP")]
        Maua = 3529401,

        [EnumValue("3529807")]
        [Description("Mineiros do Tiete - SP")]
        MineirosdoTiete = 3529807,

        [EnumValue("3530201")]
        [Description("Mirante do Paranapanema - SP")]
        MirantedoParanapanema = 3530201,

        [EnumValue("3530300")]
        [Description("Mirassol - SP")]
        Mirassol = 3530300,

        [EnumValue("3530508")]
        [Description("Mococa - SP")]
        Mococa = 3530508,

        [EnumValue("3530607")]
        [Description("Mogi das Cruzes - SP")]
        MogidasCruzes = 3530607,

        [EnumValue("3530805")]
        [Description("Mogi Mirim - SP")]
        MogiMirim = 3530805,

        [EnumValue("3531308")]
        [Description("Monte Alto - SP")]
        MonteAlto = 3531308,

        [EnumValue("3531407")]
        [Description("Monte Aprazivel - SP")]
        MonteAprazivel = 3531407,

        [EnumValue("3531506")]
        [Description("Monte Azul Paulista - SP")]
        MonteAzulPaulista = 3531506,

        [EnumValue("3531803")]
        [Description("Monte Mor - SP")]
        MonteMor = 3531803,

        [EnumValue("3531902")]
        [Description("Morro Agudo - SP")]
        MorroAgudo = 3531902,

        [EnumValue("3532009")]
        [Description("Morungaba - SP")]
        Morungaba = 3532009,

        [EnumValue("3532405")]
        [Description("Nazare Paulista - SP")]
        NazarePaulista = 3532405,

        [EnumValue("3533007")]
        [Description("Nova Granada - SP")]
        NovaGranada = 3533007,

        [EnumValue("3533403")]
        [Description("Nova Odessa - SP")]
        NovaOdessa = 3533403,

        [EnumValue("3533502")]
        [Description("Novo Horizonte - SP")]
        NovoHorizonteSP = 3533502,

        [EnumValue("3533908")]
        [Description("Olimpia - SP")]
        Olimpia = 3533908,

        [EnumValue("3534302")]
        [Description("Orlandia - SP")]
        Orlandia = 3534302,

        [EnumValue("3534401")]
        [Description("Osasco - SP")]
        Osasco = 3534401,

        [EnumValue("3534609")]
        [Description("Osvaldo Cruz - SP")]
        OsvaldoCruz = 3534609,

        [EnumValue("3534708")]
        [Description("Ourinhos - SP")]
        Ourinhos = 3534708,

        [EnumValue("3534757")]
        [Description("Ouroeste - SP")]
        Ouroeste = 3534757,

        [EnumValue("3536505")]
        [Description("Paulinia - SP")]
        Paulinia = 3536505,

        [EnumValue("3536703")]
        [Description("Pederneiras - SP")]
        Pederneiras = 3536703,

        [EnumValue("3537107")]
        [Description("Pedreira - SP")]
        Pedreira = 3537107,

        [EnumValue("3537305")]
        [Description("Penapolis - SP")]
        Penapolis = 3537305,

        [EnumValue("3537404")]
        [Description("Pereira Barreto - SP")]
        PereiraBarreto = 3537404,

        [EnumValue("3537909")]
        [Description("Pilar do Sul - SP")]
        PilardoSul = 3537909,

        [EnumValue("3538006")]
        [Description("Pindamonhangaba - SP")]
        Pindamonhangaba = 3538006,

        [EnumValue("3538709")]
        [Description("Piracicaba - SP")]
        Piracicaba = 3538709,

        [EnumValue("3538907")]
        [Description("Pirajui - SP")]
        Pirajui = 3538907,

        [EnumValue("3539004")]
        [Description("Pirangi - SP")]
        Pirangi = 3539004,

        [EnumValue("3539202")]
        [Description("Pirapozinho - SP")]
        Pirapozinho = 3539202,

        [EnumValue("3539301")]
        [Description("Pirassununga - SP")]
        Pirassununga = 3539301,

        [EnumValue("3539806")]
        [Description("Poa - SP")]
        Poa = 3539806,

        [EnumValue("3540200")]
        [Description("Pontal - SP")]
        Pontal = 3540200,

        [EnumValue("3540606")]
        [Description("Porto Feliz - SP")]
        PortoFeliz = 3540606,

        [EnumValue("3540705")]
        [Description("Porto Ferreira - SP")]
        PortoFerreira = 3540705,

        [EnumValue("3541000")]
        [Description("Praia Grande - SP")]
        PraiaGrandeSP = 3541000,

        [EnumValue("3541208")]
        [Description("Presidente Bernardes - SP")]
        PresidenteBernardes = 3541208,

        [EnumValue("3541406")]
        [Description("Presidente Prudente - SP")]
        PresidentePrudente = 3541406,

        [EnumValue("3541505")]
        [Description("Presidente Venceslau - SP")]
        PresidenteVenceslau = 3541505,

        [EnumValue("3541604")]
        [Description("Promissao - SP")]
        Promissao = 3541604,

        [EnumValue("3542602")]
        [Description("Registro - SP")]
        Registro = 3542602,

        [EnumValue("3543303")]
        [Description("Ribeirao Pires - SP")]
        RibeiraoPires = 3543303,

        [EnumValue("3543402")]
        [Description("Ribeirao Preto - SP")]
        RibeiraoPreto = 3543402,

        [EnumValue("3543907")]
        [Description("Rio Claro - SP")]
        RioClaro = 3543907,

        [EnumValue("3544004")]
        [Description("Rio das Pedras - SP")]
        RiodasPedras = 3544004,

        [EnumValue("3545159")]
        [Description("Saltinho - SP")]
        SaltinhoSP = 3545159,

        [EnumValue("3545209")]
        [Description("Salto - SP")]
        Salto = 3545209,

        [EnumValue("3545308")]
        [Description("Salto de Pirapora - SP")]
        SaltodePirapora = 3545308,

        [EnumValue("3545407")]
        [Description("Salto Grande - SP")]
        SaltoGrande = 3545407,

        [EnumValue("3545803")]
        [Description("Santa Barbara D Oeste - SP")]
        SantaBarbaraDOeste = 3545803,

        [EnumValue("3546306")]
        [Description("Santa Cruz das Palmeiras - SP")]
        SantaCruzdasPalmeiras = 3546306,

        [EnumValue("3546702")]
        [Description("Santa Gertrudes - SP")]
        SantaGertrudes = 3546702,

        [EnumValue("3547601")]
        [Description("Santa Rosa de Viterbo - SP")]
        SantaRosadeViterbo = 3547601,

        [EnumValue("3547700")]
        [Description("Santo Anastacio - SP")]
        SantoAnastacio = 3547700,

        [EnumValue("3547809")]
        [Description("Santo Andre - SP")]
        SantoAndre = 3547809,

        [EnumValue("3548005")]
        [Description("Santo Antonio de Posse - SP")]
        SantoAntoniodePosse = 3548005,

        [EnumValue("3548203")]
        [Description("Santo Antonio do Pinhal - SP")]
        SantoAntoniodoPinhal = 3548203,

        [EnumValue("3548500")]
        [Description("Santos - SP")]
        Santos = 3548500,

        [EnumValue("3548708")]
        [Description("Sao Bernardo do Campo - SP")]
        SaoBernardodoCampo = 3548708,

        [EnumValue("3548807")]
        [Description("Sao Caetano do Sul - SP")]
        SaoCaetanodoSul = 3548807,

        [EnumValue("3548906")]
        [Description("Sao Carlos - SP")]
        SaoCarlosSP = 3548906,

        [EnumValue("3549102")]
        [Description("Sao Joao da Boa Vista - SP")]
        SaoJoaodaBoaVista = 3549102,

        [EnumValue("3549409")]
        [Description("Sao Joaquim da Barra - SP")]
        SaoJoaquimdaBarra = 3549409,

        [EnumValue("3549706")]
        [Description("Sao Jose do Rio Pardo - SP")]
        SaoJosedoRioPardo = 3549706,

        [EnumValue("3549805")]
        [Description("Sao Jose do Rio Preto - SP")]
        SaoJosedoRioPreto = 3549805,

        [EnumValue("3549904")]
        [Description("Sao Jose dos Campos - SP")]
        SaoJosedosCampos = 3549904,

        [EnumValue("3550308")]
        [Description("Sao Paulo - SP")]
        SaoPaulo = 3550308,

        [EnumValue("3550407")]
        [Description("Sao Pedro - SP")]
        SaoPedro = 3550407,

        [EnumValue("3550605")]
        [Description("Sao Roque - SP")]
        SaoRoque = 3550605,

        [EnumValue("3550704")]
        [Description("Sao Sebastiao - SP")]
        SaoSebastiao = 3550704,

        [EnumValue("3551009")]
        [Description("Sao Vicente - SP")]
        SaoVicente = 3551009,

        [EnumValue("3551405")]
        [Description("Serra Azul - SP")]
        SerraAzul = 3551405,

        [EnumValue("3551504")]
        [Description("Serrana - SP")]
        Serrana = 3551504,

        [EnumValue("3551603")]
        [Description("Serra Negra - SP")]
        SerraNegra = 3551603,

        [EnumValue("3551702")]
        [Description("Sertaozinho - SP")]
        Sertaozinho = 3551702,

        [EnumValue("3552106")]
        [Description("Socorro - SP")]
        Socorro = 3552106,

        [EnumValue("3552205")]
        [Description("Sorocaba - SP")]
        Sorocaba = 3552205,

        [EnumValue("3552403")]
        [Description("Sumare - SP")]
        Sumare = 3552403,

        [EnumValue("3552502")]
        [Description("Suzano - SP")]
        Suzano = 3552502,

        [EnumValue("3552601")]
        [Description("Tabapua - SP")]
        Tabapua = 3552601,

        [EnumValue("3552700")]
        [Description("Tabatinga - SP")]
        Tabatinga = 3552700,

        [EnumValue("3552809")]
        [Description("Taboao da Serra - SP")]
        TaboaodaSerra = 3552809,

        [EnumValue("3553708")]
        [Description("Taquaritinga - SP")]
        Taquaritinga = 3553708,

        [EnumValue("3554003")]
        [Description("Tatui - SP")]
        Tatui = 3554003,

        [EnumValue("3554102")]
        [Description("Taubate - SP")]
        Taubate = 3554102,

        [EnumValue("3554300")]
        [Description("Teodoro Sampaio - SP")]
        TeodoroSampaio = 3554300,

        [EnumValue("3554508")]
        [Description("Tiete - SP")]
        Tiete = 3554508,

        [EnumValue("3555000")]
        [Description("Tupa - SP")]
        Tupa = 3555000,

        [EnumValue("3555109")]
        [Description("Tupi Paulista - SP")]
        TupiPaulista = 3555109,

        [EnumValue("3555406")]
        [Description("Ubatuba - SP")]
        Ubatuba = 3555406,

        [EnumValue("3556206")]
        [Description("Valinhos - SP")]
        Valinhos = 3556206,

        [EnumValue("3556354")]
        [Description("Vargem - SP")]
        Vargem = 3556354,

        [EnumValue("3556404")]
        [Description("Vargem Grande do Sul - SP")]
        VargemGrandedoSul = 3556404,

        [EnumValue("3556453")]
        [Description("Vargem Grande Paulista - SP")]
        VargemGrandePaulista = 3556453,

        [EnumValue("3556503")]
        [Description("Varzea Paulista - SP")]
        VarzeaPaulista = 3556503,

        [EnumValue("3556701")]
        [Description("Vinhedo - SP")]
        Vinhedo = 3556701,

        [EnumValue("3556800")]
        [Description("Viradouro - SP")]
        Viradouro = 3556800,

        [EnumValue("3556909")]
        [Description("Vista Alegre do Alto - SP")]
        VistaAlegredoAlto = 3556909,

        [EnumValue("3557006")]
        [Description("Votorantim - SP")]
        Votorantim = 3557006,

        [EnumValue("3557105")]
        [Description("Votuporanga - SP")]
        Votuporanga = 3557105,

        //Paraná
        [EnumValue("4100202")]
        [Description("Adrianopolis - PR")]
        Adrianopolis = 4100202,

        [EnumValue("4100301")]
        [Description("Agudos do Sul - PR")]
        AgudosdoSul = 4100301,

        [EnumValue("4100400")]
        [Description("Almirante Tamandare - PR")]
        AlmiranteTamandare = 4100400,

        [EnumValue("4100509")]
        [Description("Altonia - PR")]
        Altonia = 4100509,

        [EnumValue("4100608")]
        [Description("Alto Parana - PR")]
        AltoParana = 4100608,

        [EnumValue("4101002")]
        [Description("Ampere - PR")]
        Ampere = 4101002,

        [EnumValue("4101408")]
        [Description("Apucarana - PR")]
        Apucarana = 4101408,

        [EnumValue("4101507")]
        [Description("Arapongas - PR")]
        Arapongas = 4101507,

        [EnumValue("4101804")]
        [Description("Araucaria - PR")]
        Araucaria = 4101804,

        [EnumValue("4101903")]
        [Description("Assai - PR")]
        Assai = 4101903,

        [EnumValue("4102000")]
        [Description("Assis Chateaubriand - PR")]
        AssisChateaubriand = 4102000,

        [EnumValue("4102109")]
        [Description("Astorga - PR")]
        Astorga = 4102109,

        [EnumValue("4102307")]
        [Description("Balsa Nova - PR")]
        BalsaNova = 4102307,

        [EnumValue("4102406")]
        [Description("Bandeirantes - PR")]
        Bandeirantes = 4102406,

        [EnumValue("4102604")]
        [Description("Barracao - PR")]
        Barracao = 4102604,

        [EnumValue("4102752")]
        [Description("Bela Vista da Caroba - PR")]
        BelaVistadaCaroba = 4102752,

        [EnumValue("4102901")]
        [Description("Bituruna - PR")]
        Bituruna = 4102901,

        [EnumValue("4103107")]
        [Description("Bocaiuva do Sul - PR")]
        BocaiuvadoSul = 4103107,

        [EnumValue("4103156")]
        [Description("Bom Jesus do Sul - PR")]
        BomJesusdoSul = 4103156,

        [EnumValue("4103222")]
        [Description("Bom Sucesso do Sul - PR")]
        BomSucessodoSul = 4103222,

        [EnumValue("4103404")]
        [Description("Cafeara - PR")]
        Cafeara = 4103404,

        [EnumValue("4103701")]
        [Description("Cambe - PR")]
        Cambe = 4103701,

        [EnumValue("4103701")]
        [Description("Campina Grande do Sul - PR")]
        CampinaGrandedoSul = 4103701,

        [EnumValue("4104204")]
        [Description("Campo Largo - PR")]
        CampoLargo = 4104204,

        [EnumValue("4104253")]
        [Description("Campo Magro - PR")]
        CampoMagro = 4104253,

        [EnumValue("4104303")]
        [Description("Campo Mourao - PR")]
        CampoMourao = 4104303,

        [EnumValue("4104451")]
        [Description("Cantagalo - PR")]
        Cantagalo = 4104451,

        [EnumValue("4104501")]
        [Description("Capanema - PR")]
        Capanema = 4104501,

        [EnumValue("4104600")]
        [Description("Capitao Leonidas Marques - PR")]
        CapitaoLeonidasMarques = 4104600,

        [EnumValue("4104659")]
        [Description("Carambei - PR")]
        Carambei = 4104659,

        [EnumValue("4104808")]
        [Description("Cascavel - PR")]
        Cascavel = 4104808,

        [EnumValue("4104907")]
        [Description("Castro - PR")]
        Castro = 4104907,

        [EnumValue("4105003")]
        [Description("Catanduvas - PR")]
        CatanduvasPR = 4105003,

        [EnumValue("4105201")]
        [Description("Cerro Azul - PR")]
        CerroAzul = 4105201,

        [EnumValue("4105300")]
        [Description("Ceu Azul - PR")]
        CeuAzul = 4105300,

        [EnumValue("4105409")]
        [Description("Chopinzinho - PR")]
        Chopinzinho = 4105409,

        [EnumValue("4105508")]
        [Description("Cianorte - PR")]
        Cianorte = 4105508,

        [EnumValue("4105706")]
        [Description("Clevelandia - PR")]
        Clevelandia = 4105706,

        [EnumValue("4105805")]
        [Description("Colombo - PR")]
        Colombo = 4105805,

        [EnumValue("4105904")]
        [Description("Colorado - PR")]
        Colorado = 4105904,

        [EnumValue("4106209")]
        [Description("Contenda - PR")]
        Contenda = 4106209,

        [EnumValue("4106308")]
        [Description("Corbelia - PR")]
        Corbelia = 4106308,

        [EnumValue("4106407")]
        [Description("Cornelio Procopio - PR")]
        CornelioProcopio = 4106407,

        [EnumValue("4106506")]
        [Description("Coronel Vivida - PR")]
        CoronelVivida = 4106506,

        [EnumValue("4106605")]
        [Description("Cruzeiro do Oeste - PR")]
        CruzeirodoOeste = 4106605,

        [EnumValue("4106803")]
        [Description("Cruz Machado - PR")]
        CruzMachado = 4106803,

        [EnumValue("4106902")]
        [Description("Curitiba - PR")]
        Curitiba = 4106902,

        [EnumValue("4107207")]
        [Description("Dois Vizinhos - PR")]
        DoisVizinhos = 4107207,

        [EnumValue("4107256")]
        [Description("Douradina - PR")]
        Douradina = 4107256,

        [EnumValue("4107504")]
        [Description("Engenheiro Beltrao - PR")]
        EngenheiroBeltrao = 4107504,

        [EnumValue("4107652")]
        [Description("Fazenda Rio Grande - PR")]
        FazendaRioGrande = 4107652,

        [EnumValue("4107736")]
        [Description("Fernandes Pinheiro - PR")]
        FernandesPinheiro = 4107736,

        [EnumValue("4107751")]
        [Description("Figueira - PR")]
        Figueira = 4107751,

        [EnumValue("4107850")]
        [Description("Flor da Serra do Sul - PR")]
        FlordaSerradoSul = 4107850,

        [EnumValue("4108205")]
        [Description("Formosa do Oeste - PR")]
        FormosadoOeste = 4108205,

        [EnumValue("4108304")]
        [Description("Foz do Iguacu - PR")]
        FozdoIguacu = 4108304,

        [EnumValue("4108320")]
        [Description("Francisco Alves - PR")]
        FranciscoAlves = 4108320,

        [EnumValue("4108403")]
        [Description("Francisco Beltrao - PR")]
        FranciscoBeltrao = 4108403,

        [EnumValue("4108502")]
        [Description("General Carneiro - PR")]
        GeneralCarneiro = 4108502,

        [EnumValue("4108601")]
        [Description("Goioere - PR")]
        Goioere = 4108601,

        [EnumValue("4108809")]
        [Description("Guaira - PR")]
        GuairaPR = 4108809,

        [EnumValue("4109401")]
        [Description("Guarapuava - PR")]
        Guarapuava = 4109401,

        [EnumValue("4109500")]
        [Description("Guaraquecaba - PR")]
        Guaraquecaba = 4109500,

        [EnumValue("4109609")]
        [Description("Guaratuba - PR")]
        Guaratuba = 4109609,

        [EnumValue("4109807")]
        [Description("Ibipora - PR")]
        Ibipora = 4109807,

        [EnumValue("4109906")]
        [Description("Icaraima - PR")]
        Icaraima = 4109906,

        [EnumValue("4110201")]
        [Description("Inacio Martins - PR")]
        InacioMartins = 4110201,

        [EnumValue("4110607")]
        [Description("Ipora - PR")]
        IporaPR = 4110607,

        [EnumValue("4110656")]
        [Description("Iracema do Oeste - PR")]
        IracemadoOeste = 4110656,

        [EnumValue("4110706")]
        [Description("Irati - PR")]
        Irati = 4110706,

        [EnumValue("4111555")]
        [Description("Ivate - PR")]
        Ivate = 4111555,

        [EnumValue("4112009")]
        [Description("Jaguariaiva - PR")]
        Jaguariaiva = 4112009,

        [EnumValue("4112108")]
        [Description("Jandaia do Sul - PR")]
        JandaiadoSul = 4112108,

        [EnumValue("4112504")]
        [Description("Jardim Alegre - PR")]
        JardimAlegre = 4112504,

        [EnumValue("4112959")]
        [Description("Juranda - PR")]
        Juranda = 4112959,

        [EnumValue("4113205")]
        [Description("Lapa - PR")]
        Lapa = 4113205,

        [EnumValue("4113304")]
        [Description("Laranjeiras do Sul - PR")]
        LaranjeirasdoSul = 4113304,

        [EnumValue("4113452")]
        [Description("Lindoeste - PR")]
        Lindoeste = 4113452,

        [EnumValue("4113601")]
        [Description("Lobato - PR")]
        Lobato = 4113601,

        [EnumValue("4113700")]
        [Description("Londrina - PR")]
        Londrina = 4113700,

        [EnumValue("4113809")]
        [Description("Lupionopolis - PR")]
        Lupionopolis = 4113809,

        [EnumValue("4113908")]
        [Description("Mallet - PR")]
        Mallet = 4113908,

        [EnumValue("4114005")]
        [Description("Mambore - PR")]
        Mambore = 4114005,

        [EnumValue("4114104")]
        [Description("Mandaguacu - PR")]
        Mandaguacu = 4114104,

        [EnumValue("4114302")]
        [Description("Mandirituba - PR")]
        Mandirituba = 4114302,

        [EnumValue("4114401")]
        [Description("Mangueirinha - PR")]
        Mangueirinha = 4114401,

        [EnumValue("4114609")]
        [Description("Marechal Candido Rondon - PR")]
        MarechalCandidoRondon = 4114609,

        [EnumValue("4114807")]
        [Description("Marialva - PR")]
        Marialva = 4114807,

        [EnumValue("4115101")]
        [Description("Mariluz - PR")]
        Mariluz = 4115101,

        [EnumValue("4115200")]
        [Description("Maringa - PR")]
        Maringa = 4115200,

        [EnumValue("4115309")]
        [Description("Mariopolis - PR")]
        Mariopolis = 4115309,

        [EnumValue("4115408")]
        [Description("Marmeleiro - PR")]
        Marmeleiro = 4115408,

        [EnumValue("4115754")]
        [Description("Maua da Serra - PR")]
        MauadaSerra = 4115754,

        [EnumValue("4115804")]
        [Description("Medianeira - PR")]
        Medianeira = 4115804,

        [EnumValue("4116307")]
        [Description("Munhoz de Melo - PR")]
        MunhozdeMelo = 4116307,

        [EnumValue("4116703")]
        [Description("Nova Aurora - PR")]
        NovaAurora = 4116703,

        [EnumValue("4117305")]
        [Description("Ortigueira - PR")]
        Ortigueira = 4117305,

        [EnumValue("4117404")]
        [Description("Ourizona - PR")]
        Ourizona = 4117404,

        [EnumValue("4117503")]
        [Description("Paicandu - PR")]
        Paicandu = 4117503,

        [EnumValue("4117602")]
        [Description("Paicandu - PR")]
        PalmasPR = 4117602,

        [EnumValue("4117701")]
        [Description("Palmeira - PR")]
        Palmeira = 4117701,

        [EnumValue("4117909")]
        [Description("Palotina - PR")]
        Palotina = 4117909,

        [EnumValue("4118204")]
        [Description("Paranagua - PR")]
        Paranagua = 4118204,

        [EnumValue("4118402")]
        [Description("Paranavai - PR")]
        Paranavai = 4118402,

        [EnumValue("4118501")]
        [Description("Pato Branco - PR")]
        PatoBranco = 4118501,

        [EnumValue("4118600")]
        [Description("Paula Freitas - PR")]
        PaulaFreitas = 4118600,

        [EnumValue("4119004")]
        [Description("Perola D Oeste - PR")]
        PerolaDOeste = 4119004,

        [EnumValue("4119152")]
        [Description("Pinhais - PR")]
        Pinhais = 4119152,

        [EnumValue("4119400")]
        [Description("Pirai do Sul - PR")]
        PiraidoSul = 4119400,

        [EnumValue("4119905")]
        [Description("Ponta Grossa - PR")]
        PontaGrossa = 4119905,

        [EnumValue("4120002")]
        [Description("Porecatu - PR")]
        Porecatu = 4120002,

        [EnumValue("4120333")]
        [Description("Prado Ferreira - PR")]
        PradoFerreira = 4120333,

        [EnumValue("4120606")]
        [Description("Prudentopolis - PR")]
        Prudentopolis = 4120606,

        [EnumValue("4120804")]
        [Description("Quatro Barras - PR")]
        QuatroBarras = 4120804,

        [EnumValue("4120853")]
        [Description("Quatro Pontes - PR")]
        QuatroPontes = 4120853,

        [EnumValue("4121307")]
        [Description("Rancho Alegre - PR")]
        RanchoAlegre = 4121307,

        [EnumValue("4121406")]
        [Description("Realeza - PR")]
        Realeza = 4121406,

        [EnumValue("4121505")]
        [Description("Reboucas - PR")]
        Reboucas = 4121505,

        [EnumValue("4121604")]
        [Description("Renascenca - PR")]
        Renascenca = 4121604,

        [EnumValue("4122008")]
        [Description("Rio Azul - PR")]
        RioAzul = 4122008,

        [EnumValue("4122206")]
        [Description("Rio Branco do Sul - PR")]
        RioBrancodoSul = 4122206,

        [EnumValue("4122305")]
        [Description("Rio Negro - PR")]
        RioNegro = 4122305,

        [EnumValue("4122404")]
        [Description("Rolandia - PR")]
        Rolandia = 4122404,

        [EnumValue("4122602")]
        [Description("Rondon - PR")]
        Rondon = 4122602,

        [EnumValue("4122800")]
        [Description("Salgado Filho - PR")]
        SalgadoFilho = 4122800,

        [EnumValue("4123501")]
        [Description("Santa Helena - PR")]
        SantaHelenaPR = 4123501,

        [EnumValue("4123808")]
        [Description("Santa Izabel do Oeste - PR")]
        SantaIzabeldoOeste = 4123808,

        [EnumValue("4124020")]
        [Description("Santa Tereza do Oeste - PR")]
        SantaTerezadoOeste = 4124020,

        [EnumValue("4124103")]
        [Description("Santo Antonio da Platina - PR")]
        SantoAntoniodaPlatina = 4124103,

        [EnumValue("4124400")]
        [Description("Santo Antonio do Sudoeste - PR")]
        SantoAntoniodoSudoeste = 4124400,

        [EnumValue("4124806")]
        [Description("Sao Joao - PR")]
        SaoJoao = 4124806,

        [EnumValue("4125209")]
        [Description("Sao Jorge D Oeste - PR")]
        SaoJorgeDOeste = 4125209,

        [EnumValue("4125308")]
        [Description("Sao Jorge do Ivai - PR")]
        SaoJorgedoIvai = 4125308,

        [EnumValue("4125506")]
        [Description("Sao Jose dos Pinhais - PR")]
        SaoJosedosPinhais = 4125506,

        [EnumValue("4125605")]
        [Description("Sao Mateus do Sul - PR")]
        SaoMateusdoSul = 4125605,

        [EnumValue("4125704")]
        [Description("Sao Miguel do Iguacu - PR")]
        SaoMigueldoIguacu = 4125704,

        [EnumValue("4125753")]
        [Description("Sao Pedro do Iguacu - PR")]
        SaoPedrodoIguacu = 4125753,

        [EnumValue("4126009")]
        [Description("Sao Sebastiao da Amoreira - PR")]
        SaoSebastiaodaAmoreira = 4126009,

        [EnumValue("4126256")]
        [Description("Sarandi - PR")]
        SarandiPR = 4126256,

        [EnumValue("4126504")]
        [Description("Sertanopolis - PR")]
        Sertanopolis = 4126504,

        [EnumValue("4126603")]
        [Description("Siqueira Campos - PR")]
        SiqueiraCampos = 4126603,

        [EnumValue("4126652")]
        [Description("Sulina - PR")]
        Sulina = 4126652,

        [EnumValue("4127106")]
        [Description("Telemaco Borba - PR")]
        TelemacoBorba = 4127106,

        [EnumValue("4127205")]
        [Description("Terra Boa - PR")]
        TerraBoa = 4127205,

        [EnumValue("4127403")]
        [Description("Terra Roxa - PR")]
        TerraRoxa = 4127403,

        [EnumValue("4127601")]
        [Description("Tijucas do Sul - PR")]
        TijucasdoSul = 4127601,

        [EnumValue("4127700")]
        [Description("Toledo - PR")]
        Toledo = 4127700,

        [EnumValue("4127809")]
        [Description("Tomazina - PR")]
        Tomazina = 4127809,

        [EnumValue("4128104")]
        [Description("Umuarama - PR")]
        Umuarama = 4128203,

        [EnumValue("4128203")]
        [Description("Uniao da Vitoria - PR")]
        UniaodaVitoria = 4128203,

        [EnumValue("4128500")]
        [Description("Wenceslau Braz - PR")]
        WenceslauBraz = 4128500,

        [EnumValue("4128559")]
        [Description("Vera Cruz do Oeste - PR")]
        VeraCruzdoOeste = 4128559,

        [EnumValue("4128708")]
        [Description("Vitorino - PR")]
        Vitorino = 4128708,

        //Santa Catarina
        [EnumValue("4200051")]
        [Description("Abdon Batista - SC")]
        AbdonBatista = 4200051,

        [EnumValue("4200200")]
        [Description("Agrolandia - SC")]
        Agrolandia = 4200200,

        [EnumValue("4200309")]
        [Description("Agronomica - SC")]
        Agronomica = 4200309,

        [EnumValue("4200408")]
        [Description("Agua Doce - SC")]
        AguaDoce = 4200408,

        [EnumValue("4200507")]
        [Description("Aguas de Chapeco - SC")]
        AguasdeChapeco = 4200507,

        [EnumValue("4200606")]
        [Description("Aguas Mornas - SC")]
        AguasMornas = 4200606,

        [EnumValue("4200705")]
        [Description("Alfredo Wagner - SC")]
        AlfredoWagner = 4200705,

        [EnumValue("4200754")]
        [Description("Alto Bela Vista - SC")]
        AltoBelaVista = 4200754,

        [EnumValue("4200804")]
        [Description("Anchieta - SC")]
        AnchietaSC = 4200804,

        [EnumValue("4201000")]
        [Description("Anita Garibaldi - SC")]
        AnitaGaribaldi = 4201000,

        [EnumValue("4201109")]
        [Description("Anitapolis - SC")]
        Anitapolis = 4201109,

        [EnumValue("4201208")]
        [Description("Antonio Carlos - SC")]
        AntonioCarlosSC = 4201208,

        [EnumValue("4201273")]
        [Description("Arabuta - SC")]
        Arabuta = 4201273,

        [EnumValue("4201307")]
        [Description("Araquari - SC")]
        Araquari = 4201307,

        [EnumValue("4201406")]
        [Description("Ararangua - SC")]
        Ararangua = 4201406,

        [EnumValue("4201505")]
        [Description("Armazem - SC")]
        Armazem = 4201505,

        [EnumValue("4201604")]
        [Description("ArroioTrinta - SC")]
        ArroioTrinta = 4201604,

        [EnumValue("4201653")]
        [Description("Arvoredo - SC")]
        Arvoredo = 4201653,

        [EnumValue("4201703")]
        [Description("Ascurra - SC")]
        Ascurra = 4201703,

        [EnumValue("4201802")]
        [Description("Atalanta - SC")]
        Atalanta = 4201802,

        [EnumValue("4201950")]
        [Description("Balneario Arroio do Silva - SC")]
        BalnearioArroiodoSilva = 4201950,

        [EnumValue("4202008")]
        [Description("Balneario Camboriu - SC")]
        BalnearioCamboriu = 4202008,

        [EnumValue("4202073")]
        [Description("Balneario Gaivota - SC")]
        BalnearioGaivota = 4202073,

        [EnumValue("4202107")]
        [Description("Barra Velha - SC")]
        BarraVelha = 4202107,

        [EnumValue("4202131")]
        [Description("Bela Vista do Toldo - SC")]
        BelaVistadoToldo = 4202131,

        [EnumValue("4202156")]
        [Description("Belmonte - SC")]
        Belmonte = 4202156,

        [EnumValue("4202305")]
        [Description("Biguacu - SC")]
        Biguacu = 4202305,

        [EnumValue("4202404")]
        [Description("Blumenau - SC")]
        Blumenau = 4202404,

        [EnumValue("4202453")]
        [Description("Bombinhas - SC")]
        Bombinhas = 4202453,

        [EnumValue("4202503")]
        [Description("Bom Jardim da Serra - SC")]
        BomJardimdaSerra = 4202503,

        [EnumValue("4202537")]
        [Description("Bom Jesus - SC")]
        BomJesus = 4202537,

        [EnumValue("4202578")]
        [Description("Bom Jesus do Oeste - SC")]
        BomJesusdoOeste = 4202578,

        [EnumValue("4202800")]
        [Description("Braco do Norte - SC")]
        BracodoNorte = 4202800,

        [EnumValue("4202909")]
        [Description("Brusque - SC")]
        Brusque = 4202909,

        [EnumValue("4203006")]
        [Description("Cacador - SC")]
        Cacador = 4203006,

        [EnumValue("4203105")]
        [Description("Caibi - SC")]
        Caibi = 4203105,

        [EnumValue("4203204")]
        [Description("Camboriu - SC")]
        Camboriu = 4203204,

        [EnumValue("4203303")]
        [Description("Campo Alegre - SC")]
        CampoAlegre = 4203303,

        [EnumValue("4203501")]
        [Description("Campo Ere - SC")]
        CampoEre = 4203501,

        [EnumValue("4203600")]
        [Description("Campos Novos - SC")]
        CamposNovos = 4203600,

        [EnumValue("4203709")]
        [Description("Canelinha - SC")]
        Canelinha = 4203709,

        [EnumValue("4203808")]
        [Description("Canoinhas - SC")]
        Canoinhas = 4203808,

        [EnumValue("4203907")]
        [Description("Capinzal - SC")]
        Capinzal = 4203907,

        [EnumValue("4204004")]
        [Description("Catanduvas - SC")]
        CatanduvasSC = 4204004,

        [EnumValue("4204152")]
        [Description("Celso Ramos - SC")]
        CelsoRamos = 4204152,

        [EnumValue("4204194")]
        [Description("Chapadao do Lageado - SC")]
        ChapadaodoLageado = 4204194,

        [EnumValue("4204202")]
        [Description("Chapeco - SC")]
        Chapeco = 4204202,

        [EnumValue("4204251")]
        [Description("Cocal do Sul - SC")]
        CocaldoSul = 4204251,

        [EnumValue("4204301")]
        [Description("Concordia - SC")]
        Concordia = 4204301,

        [EnumValue("4204350")]
        [Description("Cordilheira Alta - SC")]
        CordilheiraAlta = 4204350,

        [EnumValue("4204400")]
        [Description("Coronel Freitas - SC")]
        CoronelFreitas = 4204400,

        [EnumValue("4204509")]
        [Description("Corupa - SC")]
        Corupa = 4204509,

        [EnumValue("4204558")]
        [Description("Correia Pinto - SC")]
        CorreiaPinto = 4204558,

        [EnumValue("4204608")]
        [Description("Criciuma - SC")]
        Criciuma = 4204608,

        [EnumValue("4204707")]
        [Description("Cunha Pora - SC")]
        CunhaPora = 4204707,

        [EnumValue("4204756")]
        [Description("Cunhatai - SC")]
        Cunhatai = 4204756,

        [EnumValue("4204806")]
        [Description("Curitibanos - SC")]
        Curitibanos = 4204806,

        [EnumValue("4204905")]
        [Description("Descanso - SC")]
        Descanso = 4204905,

        [EnumValue("4205001")]
        [Description("Dionisio Cerqueira - SC")]
        DionisioCerqueira = 4205001,

        [EnumValue("4205100")]
        [Description("Dona Emma - SC")]
        DonaEmma = 4205100,

        [EnumValue("4205159")]
        [Description("Doutor Pedrinho - SC")]
        DoutorPedrinho = 4205159,

        [EnumValue("4205191")]
        [Description("Ermo - SC")]
        Ermo = 4205191,

        [EnumValue("4205209")]
        [Description("Erval Velho - SC")]
        ErvalVelho = 4205209,

        [EnumValue("4205308")]
        [Description("Faxinal Dos Guedes - SC")]
        FaxinalDosGuedes = 4205308,

        [EnumValue("4205407")]
        [Description("Florianopolis - SC")]
        Florianopolis = 4205407,

        [EnumValue("4205456")]
        [Description("Forquilhinha - SC")]
        Forquilhinha = 4205456,

        [EnumValue("4205506")]
        [Description("Fraiburgo - SC")]
        Fraiburgo = 4205506,

        [EnumValue("4205555")]
        [Description("Frei Rogerio - SC")]
        FreiRogerio = 4205555,

        [EnumValue("4205704")]
        [Description("Garopaba - SC")]
        Garopaba = 4205704,

        [EnumValue("4205902")]
        [Description("Gaspar - SC")]
        Gaspar = 4205902,

        [EnumValue("4206108")]
        [Description("Grao Para - SC")]
        GraoPara = 4206108,

        [EnumValue("4206207")]
        [Description("Gravatal - SC")]
        Gravatal = 4206207,

        [EnumValue("4206405")]
        [Description("Guaraciaba - SC")]
        Guaraciaba = 4206405,

        [EnumValue("4206504")]
        [Description("Guaramirim - SC")]
        Guaramirim = 4206504,

        [EnumValue("4206603")]
        [Description("Guaruja do Sul - SC")]
        GuarujadoSul = 4206603,

        [EnumValue("4206652")]
        [Description("Guatambu - SC")]
        Guatambu = 4206652,

        [EnumValue("4206702")]
        [Description("Herval D Oeste - SC")]
        HervalOeste = 4206702,

        [EnumValue("4206751")]
        [Description("Ibiam - SC")]
        Ibiam = 4206751,

        [EnumValue("4206801")]
        [Description("Ibicare - SC")]
        Ibicare = 4206801,

        [EnumValue("4206900")]
        [Description("Ibirama - SC")]
        Ibirama = 4206900,

        [EnumValue("4207007")]
        [Description("Icara - SC")]
        Icara = 4207007,

        [EnumValue("4207205")]
        [Description("Imarui - SC")]
        Imarui = 4207205,

        [EnumValue("4207304")]
        [Description("Imbituba - SC")]
        Imbituba = 4207304,

        [EnumValue("4207403")]
        [Description("Imbuia - SC")]
        Imbuia = 4207403,

        [EnumValue("4207502")]
        [Description("Indaial - SC")]
        Indaial = 4207502,

        [EnumValue("4207601")]
        [Description("Ipira - SC")]
        Ipira = 4207601,

        [EnumValue("4207650")]
        [Description("Ipora do Oeste - SC")]
        IporadoOeste = 4207650,

        [EnumValue("4207684")]
        [Description("Ipuacu - SC")]
        Ipuacu = 4207684,

        [EnumValue("4207700")]
        [Description("Ipumirim - SC")]
        Ipumirim = 4207700,

        [EnumValue("4207809")]
        [Description("Irani - SC")]
        Irani = 4207809,

        [EnumValue("4207908")]
        [Description("Irineopolis - SC")]
        Irineopolis = 4207908,

        [EnumValue("4208005")]
        [Description("Ita - SC")]
        Ita = 4208005,

        [EnumValue("4208203")]
        [Description("Itajai - SC")]
        Itajai = 4208203,

        [EnumValue("4208302")]
        [Description("Itapema - SC")]
        Itapema = 4208302,

        [EnumValue("4208401")]
        [Description("Itapiranga - SC")]
        Itapiranga = 4208401,

        [EnumValue("4208500")]
        [Description("Ituporanga - SC")]
        Ituporanga = 4208500,

        [EnumValue("4208708")]
        [Description("Jacinto Machado - SC")]
        JacintoMachado = 4208708,

        [EnumValue("4208807")]
        [Description("Jaguaruna - SC")]
        Jaguaruna = 4208807,

        [EnumValue("4208906")]
        [Description("Jaragua do Sul - SC")]
        JaraguadoSul = 4208906,

        [EnumValue("4208955")]
        [Description("Jardinopolis - SC")]
        JardinopolisSC = 4208955,

        [EnumValue("4209003")]
        [Description("Joacaba - SC")]
        Joacaba = 4209003,

        [EnumValue("4209102")]
        [Description("Joinville - SC")]
        Joinville = 4209102,

        [EnumValue("4209177")]
        [Description("Jupia - SC")]
        Jupia = 4209177,

        [EnumValue("4209201")]
        [Description("Lacerdopolis - SC")]
        Lacerdopolis = 4209201,

        [EnumValue("4209300")]
        [Description("Lages - SC")]
        Lages = 4209300,

        [EnumValue("4209409")]
        [Description("Laguna - SC")]
        Laguna = 4209409,

        [EnumValue("4209458")]
        [Description("Lajeado Grande - SC")]
        LajeadoGrande = 4209458,

        [EnumValue("4209607")]
        [Description("Lauro Muller - SC")]
        LauroMuller = 4209607,

        [EnumValue("4209706")]
        [Description("Lebon Regis - SC")]
        LebonRegis = 4209706,

        [EnumValue("4209854")]
        [Description("Lindoia do Sul - SC")]
        LindoiadoSul = 4209854,

        [EnumValue("4209904")]
        [Description("Lontras - SC")]
        Lontras = 4209904,

        [EnumValue("4210001")]
        [Description("Luiz Alves - SC")]
        LuizAlves = 4210001,

        [EnumValue("4210035")]
        [Description("Luzerna - SC")]
        Luzerna = 4210035,

        [EnumValue("4210050")]
        [Description("Macieira - SC")]
        Macieira = 4210050,

        [EnumValue("4210100")]
        [Description("Mafra - SC")]
        Mafra = 4210100,

        [EnumValue("4210209")]
        [Description("Major Gercino - SC")]
        MajorGercino = 4210209,

        [EnumValue("4210308")]
        [Description("Major Vieira - SC")]
        MajorVieira = 4210308,

        [EnumValue("4210407")]
        [Description("Maracaja - SC")]
        Maracaja = 4210407,

        [EnumValue("4210506")]
        [Description("Maravilha - SC")]
        Maravilha = 4210506,

        [EnumValue("4210555")]
        [Description("Marema - SC")]
        Marema = 4210555,

        [EnumValue("4210605")]
        [Description("Massaranduba - SC")]
        Massaranduba = 4210605,

        [EnumValue("4210803")]
        [Description("Meleiro - SC")]
        Meleiro = 4210803,

        [EnumValue("4210902")]
        [Description("Modelo - SC")]
        Modelo = 4210902,

        [EnumValue("4211009")]
        [Description("Mondai - SC")]
        Mondai = 4211009,

        [EnumValue("4211058")]
        [Description("Monte Carlo - SC")]
        MonteCarlo = 4211058,

        [EnumValue("4211108")]
        [Description("Monte Castelo - SC")]
        MonteCastelo = 4211108,

        [EnumValue("4211207")]
        [Description("Morro da Fumaca - SC")]
        MorrodaFumaca = 4211207,

        [EnumValue("4211256")]
        [Description("Morro Grande - SC")]
        MorroGrande = 4211256,

        [EnumValue("4211306")]
        [Description("Navegantes - SC")]
        Navegantes = 4211306,

        [EnumValue("4211405")]
        [Description("Nova Erechim - SC")]
        NovaErechim = 4211405,

        [EnumValue("4211454")]
        [Description("Nova Itaberaba - SC")]
        NovaItaberaba = 4211454,

        [EnumValue("4211504")]
        [Description("Nova Trento - SC")]
        NovaTrento = 4211504,

        [EnumValue("4211603")]
        [Description("Nova Veneza - SC")]
        NovaVenezaSC = 4211603,

        [EnumValue("4211652")]
        [Description("Novo Horizonte - SC")]
        NovoHorizonteSC = 4211652,

        [EnumValue("4211702")]
        [Description("Orleans - SC")]
        Orleans = 4211702,

        [EnumValue("4211751")]
        [Description("Otacilio Costa - SC")]
        OtacilioCosta = 4211751,

        [EnumValue("4211801")]
        [Description("Ouro - SC")]
        Ouro = 4211801,

        [EnumValue("4211876")]
        [Description("Paial - SC")]
        Paial = 4211876,

        [EnumValue("4211900")]
        [Description("Palhoca - SC")]
        Palhoca = 4211900,

        [EnumValue("4212007")]
        [Description("Palma Sola - SC")]
        PalmaSola = 4212007,

        [EnumValue("4212106")]
        [Description("Palmitos - SC")]
        Palmitos = 4212106,

        [EnumValue("4212205")]
        [Description("Papanduva - SC")]
        Papanduva = 4212205,

        [EnumValue("4212254")]
        [Description("Passo de Torres - SC")]
        PassodeTorres = 4212254,

        [EnumValue("4212270")]
        [Description("Passos Maia - SC")]
        PassosMaia = 4212270,

        [EnumValue("4212304")]
        [Description("Paulo Lopes - SC")]
        PauloLopes = 4212304,

        [EnumValue("4212403")]
        [Description("Pedras Grandes - SC")]
        PedrasGrandes = 4212403,

        [EnumValue("4212502")]
        [Description("Penha - SC")]
        Penha = 4212502,

        [EnumValue("4212601")]
        [Description("Peritiba - SC")]
        Peritiba = 4212601,

        [EnumValue("4212650")]
        [Description("Pescaria Brava - SC")]
        PescariaBrava = 4212650,

        [EnumValue("4212700")]
        [Description("Petrolandia - SC")]
        Petrolandia = 4212700,

        [EnumValue("4212908")]
        [Description("Pinhalzinho - SC")]
        Pinhalzinho = 4212908,

        [EnumValue("4213104")]
        [Description("Piratuba - SC")]
        Piratuba = 4213104,

        [EnumValue("4213153")]
        [Description("Planalto Alegre - SC")]
        PlanaltoAlegre = 4213153,

        [EnumValue("4213203")]
        [Description("Pomerode - SC")]
        Pomerode = 4213203,

        [EnumValue("4213302")]
        [Description("Ponte Alta - SC")]
        PonteAlta = 4213302,

        [EnumValue("4213351")]
        [Description("Ponte Alta do Norte - SC")]
        PonteAltadoNorte = 4213351,

        [EnumValue("4213401")]
        [Description("Ponte Serrada - SC")]
        PonteSerrada = 4213401,

        [EnumValue("4213500")]
        [Description("Porto Belo - SC")]
        PortoBelo = 4213500,

        [EnumValue("4213609")]
        [Description("Porto Uniao - SC")]
        PortoUniao = 4213609,

        [EnumValue("4213708")]
        [Description("Pouso Redondo - SC")]
        PousoRedondo = 4213708,

        [EnumValue("4213807")]
        [Description("Praia Grande - SC")]
        PraiaGrandeSC = 4213807,

        [EnumValue("4214003")]
        [Description("Presidente Getulio - SC")]
        PresidenteGetulio = 4214003,

        [EnumValue("4214102")]
        [Description("Presidente Nereu - SC")]
        PresidenteNereu = 4214102,

        [EnumValue("4214151")]
        [Description("Princesa - SC")]
        Princesa = 4214151,

        [EnumValue("4214201")]
        [Description("Quilombo - SC")]
        Quilombo = 4214201,

        [EnumValue("4214300")]
        [Description("Rancho Queimado - SC")]
        RanchoQueimado = 4214300,

        [EnumValue("4214409")]
        [Description("Rio das Antas - SC")]
        RiodasAntas = 4214409,

        [EnumValue("4214607")]
        [Description("Rio do Oeste - SC")]
        RiodoOeste = 4214607,

        [EnumValue("4214706")]
        [Description("Rio dos Cedros - SC")]
        RiodosCedros = 4214706,

        [EnumValue("4214805")]
        [Description("Rio do Sul - SC")]
        RiodoSul = 4214805,

        [EnumValue("4214904")]
        [Description("Rio Fortuna - SC")]
        RioFortuna = 4214904,

        [EnumValue("4215000")]
        [Description("Rio Negrinho - SC")]
        RioNegrinho = 4215000,

        [EnumValue("4215059")]
        [Description("Rio Rufino - SC")]
        RioRufino = 4215059,

        [EnumValue("4215075")]
        [Description("Riqueza - SC")]
        Riqueza = 4215075,

        [EnumValue("4215356")]
        [Description("Saltinho - SC")]
        SaltinhoSC = 4215356,

        [EnumValue("4215406")]
        [Description("Salto Veloso - SC")]
        SaltoVeloso = 4215406,

        [EnumValue("4215455")]
        [Description("Sangao - SC")]
        Sangao = 4215455,

        [EnumValue("4215505")]
        [Description("Santa Cecilia - SC")]
        SantaCecilia = 4215505,

        [EnumValue("4215554")]
        [Description("Santa Helena - SC")]
        SantaHelenaSC = 4215554,

        [EnumValue("4215604")]
        [Description("Santa Rosa de Lima - SC")]
        SantaRosadeLima = 4215604,

        [EnumValue("4215653")]
        [Description("Santa Rosa do Sul - SC")]
        SantaRosadoSul = 4215653,

        [EnumValue("4215679")]
        [Description("Santa Terezinha - SC")]
        SantaTerezinha = 4215679,

        [EnumValue("4215695")]
        [Description("Santiago do Sul - SC")]
        SantiagodoSul = 4215695,

        [EnumValue("4215703")]
        [Description("Santo Amaro da Imperatriz - SC")]
        SantoAmarodaImperatriz = 4215703,

        [EnumValue("4215802")]
        [Description("Sao Bento do Sul - SC")]
        SaoBentodoSul = 4215802,

        [EnumValue("4215901")]
        [Description("Sao Bonifacio - SC")]
        SaoBonifacio = 4215901,

        [EnumValue("4216008")]
        [Description("Sao Carlos - SC")]
        SaoCarlosSC = 4216008,

        [EnumValue("4216057")]
        [Description("Sao Cristovao do Sul - SC")]
        SaoCristovaodoSul = 4216057,

        [EnumValue("4216107")]
        [Description("Sao Domingos - SC")]
        SaoDomingos = 4216107,

        [EnumValue("4216206")]
        [Description("Sao Francisco do Sul - SC")]
        SaoFranciscodoSul = 4216206,

        [EnumValue("4216255")]
        [Description("Sao Joao do Oeste - SC")]
        SaoJoaodoOeste = 4216255,

        [EnumValue("4216305")]
        [Description("Sao Joao Batista - SC")]
        SaoJoaoBatista = 4216305,

        [EnumValue("4216354")]
        [Description("Sao Joao do Itaperiu - SC")]
        SaoJoaodoItaperiu = 4216354,

        [EnumValue("4216404")]
        [Description("Sao Joao do Sul - SC")]
        SaoJoaodoSul = 4216404,

        [EnumValue("4216503")]
        [Description("Sao Joaquim - SC")]
        SaoJoaquim = 4216503,

        [EnumValue("4216602")]
        [Description("Sao Jose - SC")]
        SaoJose = 4216602,

        [EnumValue("4216800")]
        [Description("Sao Jose do Cerrito - SC")]
        SaoJosedoCerrito = 4216800,

        [EnumValue("4216909")]
        [Description("Sao Lourenco do Oeste - SC")]
        SaoLourencodoOeste = 4216909,

        [EnumValue("4217006")]
        [Description("Sao Ludgero - SC")]
        SaoLudgero = 4217006,

        [EnumValue("4217105")]
        [Description("Sao Martinho - SC")]
        SaoMartinho = 4217105,

        [EnumValue("4217204")]
        [Description("Sao Miguel do Oeste - SC")]
        SaoMigueldoOeste = 4217204,

        [EnumValue("4217303")]
        [Description("Saudades - SC")]
        Saudades = 4217303,

        [EnumValue("4217402")]
        [Description("Schroeder - SC")]
        Schroeder = 4217402,

        [EnumValue("4217501")]
        [Description("Seara - SC")]
        Seara = 4217501,

        [EnumValue("4217550")]
        [Description("Serra Alta - SC")]
        SerraAlta = 4217550,

        [EnumValue("4217600")]
        [Description("Sideropolis - SC")]
        Sideropolis = 4217600,

        [EnumValue("4217709")]
        [Description("Sombrio - SC")]
        Sombrio = 4217709,

        [EnumValue("4217808")]
        [Description("Taio - SC")]
        Taio = 4217808,

        [EnumValue("4217907")]
        [Description("Tangara - SC")]
        Tangara = 4217907,

        [EnumValue("4218004")]
        [Description("Tijucas - SC")]
        Tijucas = 4218004,

        [EnumValue("4218202")]
        [Description("Timbo - SC")]
        Timbo = 4218202,

        [EnumValue("4218251")]
        [Description("Timbo Grande - SC")]
        TimboGrande = 4218251,

        [EnumValue("4218301")]
        [Description("Tres Barras - SC")]
        TresBarras = 4218301,

        [EnumValue("4218350")]
        [Description("Treviso - SC")]
        Treviso = 4218350,

        [EnumValue("4218400")]
        [Description("Treze de Maio - SC")]
        TrezedeMaio = 4218400,

        [EnumValue("4218509")]
        [Description("Treze Tilias - SC")]
        TrezeTilias = 4218509,

        [EnumValue("4218608")]
        [Description("Trombudo Central - SC")]
        TrombudoCentral = 4218608,

        [EnumValue("4218707")]
        [Description("Tubarao - SC")]
        Tubarao = 4218707,

        [EnumValue("4218756")]
        [Description("Tunapolis - SC")]
        Tunapolis = 4218756,

        [EnumValue("4218806")]
        [Description("Turvo - SC")]
        Turvo = 4218806,

        [EnumValue("4218855")]
        [Description("Uniao do Oeste - SC")]
        UniaodoOeste = 4218855,

        [EnumValue("4219002")]
        [Description("Urussanga - SC")]
        Urussanga = 4219002,

        [EnumValue("4219101")]
        [Description("Vargeao - SC")]
        Vargeao = 4219101,

        [EnumValue("4219176")]
        [Description("Vargem Bonita - SC")]
        VargemBonita = 4219176,

        [EnumValue("4219200")]
        [Description("Vidal Ramos - SC")]
        VidalRamos = 4219200,

        [EnumValue("4219309")]
        [Description("Videira - SC")]
        Videira = 4219309,

        [EnumValue("4219507")]
        [Description("Xanxere - SC")]
        Xanxere = 4219507,

        [EnumValue("4219606")]
        [Description("Xavantina - SC")]
        Xavantina = 4219606,

        [EnumValue("4219705")]
        [Description("Xaxim - SC")]
        Xaxim = 4219705,

        [EnumValue("4219853")]
        [Description("Zortea - SC")]
        Zortea = 4219853,

        [EnumValue("4220000")]
        [Description("Balneario Rincao - SC")]
        BalnearioRincao = 4220000,

        //Rio Grande do Sul
        [EnumValue("4300059")]
        [Description("Agua Santa - RS")]
        AguaSanta = 4300059,

        [EnumValue("4300406")]
        [Description("Alegrete - RS")]
        Alegrete = 4300406,

        [EnumValue("4300505")]
        [Description("Alpestre - RS")]
        Alpestre = 4300505,

        [EnumValue("4300570")]
        [Description("Alto Feliz - RS")]
        AltoFeliz = 4300570,

        [EnumValue("4300604")]
        [Description("Alvorada - RS")]
        Alvorada = 4300604,

        [EnumValue("4300638")]
        [Description("Amaral Ferrador - RS")]
        AmaralFerrador = 4300638,

        [EnumValue("4300646")]
        [Description("Ametista do Sul - RS")]
        AmetistadoSul = 4300646,

        [EnumValue("4300802")]
        [Description("Antonio Prado - RS")]
        AntonioPrado = 4300802,

        [EnumValue("4301008")]
        [Description("Arroio do Meio - RS")]
        ArroiodoMeio = 4301008,

        [EnumValue("4301057")]
        [Description("Arroio do Sal - RS")]
        ArroiodoSal = 4301057,

        [EnumValue("4301107")]
        [Description("Arroio dos Ratos - RS")]
        ArroiodosRatos = 4301107,

        [EnumValue("4301602")]
        [Description("Bage - RS")]
        Bage = 4301602,

        [EnumValue("4301636")]
        [Description("Balneario Pinhal - RS")]
        BalnearioPinhal = 4301636,

        [EnumValue("4301701")]
        [Description("Barao de Cotergipe - RS")]
        BaraodeCotergipe = 4301701,

        [EnumValue("4301958")]
        [Description("Barra Funda - RS")]
        BarraFunda = 4301958,

        [EnumValue("4302105")]
        [Description("Bento Goncalves - RS")]
        BentoGoncalves = 4302105,

        [EnumValue("4302204")]
        [Description("Boa vista do Burica - RS")]
        BoavistadoBurica = 4302204,

        [EnumValue("4302352")]
        [Description("Bom Principio - RS")]
        BomPrincipio = 4302352,

        [EnumValue("4302451")]
        [Description("Boqueirao do Leao - RS")]
        BoqueiraodoLeao = 4302451,

        [EnumValue("4302808")]
        [Description("Cacapava do Sul - RS")]
        CacapavadoSul = 4302808,

        [EnumValue("4303004")]
        [Description("Cachoeira do Sul - RS")]
        CachoeiradoSul = 4303004,

        [EnumValue("4303103")]
        [Description("Cachoeirinha - RS")]
        Cachoeirinha = 4303103,

        [EnumValue("4303301")]
        [Description("Caibate - RS")]
        Caibate = 4303301,

        [EnumValue("4303400")]
        [Description("Caicara - RS")]
        Caicara = 4303400,

        [EnumValue("4303509")]
        [Description("Camaqua - RS")]
        Camaqua = 4303509,

        [EnumValue("4303673")]
        [Description("Campestre da Serra - RS")]
        CampestredaSerra = 4303673,

        [EnumValue("4303806")]
        [Description("Campinas do Sul - RS")]
        CampinasdoSul = 4303806,

        [EnumValue("4303905")]
        [Description("Campo Bom - RS")]
        CampoBom = 4303905,

        [EnumValue("4304002")]
        [Description("Campo Novo - RS")]
        CampoNovo = 4304002,

        [EnumValue("4304358")]
        [Description("Candiota - RS")]
        Candiota = 4304358,

        [EnumValue("4304408")]
        [Description("Canela - RS")]
        Canela = 4304408,

        [EnumValue("4304507")]
        [Description("Cangucu - RS")]
        Cangucu = 4304507,

        [EnumValue("4304606")]
        [Description("Canoas - RS")]
        Canoas = 4304606,

        [EnumValue("4304630")]
        [Description("Capao da Canoa - RS")]
        CapaodaCanoa = 4304630,

        [EnumValue("4304705")]
        [Description("Carazinho - RS")]
        Carazinho = 4304705,

        [EnumValue("4304804")]
        [Description("Carlos Barbosa - RS")]
        CarlosBarbosa = 4304804,

        [EnumValue("4304903")]
        [Description("Casca - RS")]
        Casca = 4304903,

        [EnumValue("4305108")]
        [Description("Caxias do Sul - RS")]
        CaxiasdoSul = 4305108,

        [EnumValue("4305173")]
        [Description("Cerro Grande do Sul - RS")]
        CerroGrandedoSul = 4305173,

        [EnumValue("4305207")]
        [Description("Cerro Largo - RS")]
        CerroLargo = 4305207,

        [EnumValue("4305306")]
        [Description("Chapada - RS")]
        Chapada = 4305306,

        [EnumValue("4305355")]
        [Description("Charqueadas - RS")]
        Charqueadas = 4305355,

        [EnumValue("4305702")]
        [Description("Condor - RS")]
        Condor = 4305702,

        [EnumValue("4305801")]
        [Description("Constantina - RS")]
        Constantina = 4305801,

        [EnumValue("4305850")]
        [Description("Coqueiros do Sul - RS")]
        CoqueirosdoSul = 4305850,

        [EnumValue("4306007")]
        [Description("Crissiumal - RS")]
        Crissiumal = 4306007,

        [EnumValue("4306072")]
        [Description("Cristal do Sul - RS")]
        CristaldoSul = 4306072,

        [EnumValue("4306106")]
        [Description("Cruz Alta - RS")]
        CruzAlta = 4306106,

        [EnumValue("4306304")]
        [Description("David Canabarro - RS")]
        DavidCanabarro = 4306304,

        [EnumValue("4306403")]
        [Description("Dois Irmaos - RS")]
        DoisIrmaos = 4306403,

        [EnumValue("4306502")]
        [Description("Dom Feliciano - RS")]
        DomFeliciano = 4306502,

        [EnumValue("4306601")]
        [Description("Dom Pedrito - RS")]
        DomPedrito = 4306601,

        [EnumValue("4306734")]
        [Description("Doutor Mauricio Cardoso - RS")]
        DoutorMauricioCardoso = 4306734,

        [EnumValue("4306767")]
        [Description("Eldorado do Sul - RS")]
        EldoradodoSul = 4306767,

        [EnumValue("4306809")]
        [Description("Encantado - RS")]
        Encantado = 4306809,

        [EnumValue("4307005")]
        [Description("Erechim - RS")]
        Erechim = 4307005,

        [EnumValue("4307054")]
        [Description("Ernestina - RS")]
        Ernestina = 4307054,

        [EnumValue("4307500")]
        [Description("Espumoso - RS")]
        Espumoso = 4307500,

        [EnumValue("4307559")]
        [Description("Estacao - RS")]
        Estacao = 4307559,

        [EnumValue("4307609")]
        [Description("Estancia Velha - RS")]
        EstanciaVelha = 4307609,

        [EnumValue("4307708")]
        [Description("Esteio - RS")]
        Esteio = 4307708,

        [EnumValue("4307807")]
        [Description("Estrela - RS")]
        Estrela = 4307807,

        [EnumValue("4307906")]
        [Description("Farroupilha - RS")]
        Farroupilha = 4307906,

        [EnumValue("4308102")]
        [Description("Feliz - RS")]
        Feliz = 4308102,

        [EnumValue("4308201")]
        [Description("Flores da Cunha - RS")]
        FloresdaCunha = 4308201,

        [EnumValue("4308458")]
        [Description("Fortaleza dos Valos - RS")]
        FortalezadosValos = 4308458,

        [EnumValue("4308508")]
        [Description("Frederico Westphalen - RS")]
        FredericoWestphalen = 4308508,

        [EnumValue("4308607")]
        [Description("Garibaldi - RS")]
        Garibaldi = 4308607,

        [EnumValue("4308706")]
        [Description("Gaurama - RS")]
        Gaurama = 4308706,

        [EnumValue("4308904")]
        [Description("Getulio Vargas - RS")]
        GetulioVargas = 4308904,

        [EnumValue("4309001")]
        [Description("Girua - RS")]
        Girua = 4309001,

        [EnumValue("4309050")]
        [Description("Glorinha - RS")]
        Glorinha = 4309050,

        [EnumValue("4309100")]
        [Description("Gramado - RS")]
        Gramado = 4309100,

        [EnumValue("4309209")]
        [Description("Gravatai - RS")]
        Gravatai = 4309209,

        [EnumValue("4309308")]
        [Description("Guaiba - RS")]
        Guaiba = 4309308,

        [EnumValue("4309407")]
        [Description("Guapore - RS")]
        Guapore = 4309407,

        [EnumValue("4309506")]
        [Description("Guarani das Missoes - RS")]
        GuaranidasMissoes = 4309506,

        [EnumValue("4309605")]
        [Description("Horizontina - RS")]
        Horizontina = 4309605,

        [EnumValue("4309803")]
        [Description("Ibiaca - RS")]
        Ibiaca = 4309803,

        [EnumValue("4309902")]
        [Description("Ibiraiaras - RS")]
        Ibiraiaras = 4309902,

        [EnumValue("4310009")]
        [Description("Ibiruba - RS")]
        Ibiruba = 4310009,

        [EnumValue("4310207")]
        [Description("Ijui - RS")]
        Ijui = 4310207,

        [EnumValue("4310330")]
        [Description("Imbe - RS")]
        Imbe = 4310330,

        [EnumValue("4310405")]
        [Description("Independencia - RS")]
        Independencia = 4310405,

        [EnumValue("4310439")]
        [Description("Ipe - RS")]
        Ipe = 4310439,

        [EnumValue("4310504")]
        [Description("Irai - RS")]
        Irai = 4310504,

        [EnumValue("4310603")]
        [Description("Itaqui - RS")]
        Itaqui = 4310603,

        [EnumValue("4310801")]
        [Description("Ivoti - RS")]
        Ivoti = 4310801,

        [EnumValue("4310850")]
        [Description("Jaboticaba - RS")]
        Jaboticaba = 4310850,

        [EnumValue("4310900")]
        [Description("Jacutinga - RS")]
        Jacutinga = 4310900,

        [EnumValue("4311007")]
        [Description("Jaguarao - RS")]
        Jaguarao = 4311007,

        [EnumValue("4311205")]
        [Description("Julio de Castilhos - RS")]
        JuliodeCastilhos = 4311205,

        [EnumValue("4311239")]
        [Description("Lagoa Bonita do Sul - RS")]
        LagoaBonitadoSul = 4311239,

        [EnumValue("4311270")]
        [Description("Lagoa dos Tres Cantos - RS")]
        LagoadosTresCantos = 4311270,

        [EnumValue("4311304")]
        [Description("Lagoa Vermelha - RS")]
        LagoaVermelha = 4311304,

        [EnumValue("4311403")]
        [Description("Lajeado - RS")]
        Lajeado = 4311403,

        [EnumValue("4311809")]
        [Description("Marau - RS")]
        Marau = 4311809,

        [EnumValue("4312385")]
        [Description("Monte Belo do Sul - RS")]
        MonteBelodoSul = 4312385,

        [EnumValue("4312401")]
        [Description("Montenegro - RS")]
        Montenegro = 4312401,

        [EnumValue("4312450")]
        [Description("Morro Redondo - RS")]
        MorroRedondo = 4312450,

        [EnumValue("4312476")]
        [Description("Morro Reuter - RS")]
        MorroReuter = 4312476,

        [EnumValue("4312609")]
        [Description("Mucum - RS")]
        Mucum = 4312609,

        [EnumValue("4312658")]
        [Description("Nao-Me-Toque - RS")]
        NaoMeToque = 4312658,

        [EnumValue("4312674")]
        [Description("Nicolau Vergueiro - RS")]
        NicolauVergueiro = 4312674,

        [EnumValue("4312708")]
        [Description("Nonoai - RS")]
        Nonoai = 4312708,

        [EnumValue("4312757")]
        [Description("Nova Alvorada - RS")]
        NovaAlvorada = 4312757,

        [EnumValue("4312807")]
        [Description("Nova Araca - RS")]
        NovaAraca = 4312807,

        [EnumValue("4312906")]
        [Description("Nova Bassano - RS")]
        NovaBassano = 4312906,

        [EnumValue("4312955")]
        [Description("Nova Boa Vista - RS")]
        NovaBoaVista = 4312955,

        [EnumValue("4313037")]
        [Description("Nova Esperanca do Sul - RS")]
        NovaEsperancadoSul = 4313037,

        [EnumValue("4313201")]
        [Description("Nova Petropolis - RS")]
        NovaPetropolis = 4313201,

        [EnumValue("4313300")]
        [Description("Nova Prata - RS")]
        NovaPrata = 4313300,

        [EnumValue("4313409")]
        [Description("Nova Hamburgo - RS")]
        NovaHamburgo = 4313409,

        [EnumValue("4313490")]
        [Description("Nova Barreiro - RS")]
        NovaBarreiro = 4313490,

        [EnumValue("4313508")]
        [Description("Osorio - RS")]
        Osorio = 4313508,

        [EnumValue("4313706")]
        [Description("Palmeira das Missoes - RS")]
        PalmeiradasMissoes = 4313706,

        [EnumValue("4313805")]
        [Description("Palmitinho - RS")]
        Palmitinho = 4313805,

        [EnumValue("4313904")]
        [Description("Panambi - RS")]
        Panambi = 4313904,

        [EnumValue("4313953")]
        [Description("Pantano Grande - RS")]
        PantanoGrande = 4313953,

        [EnumValue("4314001")]
        [Description("Parai - RS")]
        Parai = 4314001,

        [EnumValue("4314050")]
        [Description("Parobe - RS")]
        Parobe = 4314050,

        [EnumValue("4314100")]
        [Description("Passo Fundo - RS")]
        PassoFundo = 4314100,

        [EnumValue("4314407")]
        [Description("Pelotas - RS")]
        Pelotas = 4314407,

        [EnumValue("4314423")]
        [Description("Picada Cafe - RS")]
        PicadaCafe = 4314423,

        [EnumValue("4314456")]
        [Description("Pinhal - RS")]
        Pinhal = 4314456,

        [EnumValue("4314704")]
        [Description("Planalto - RS")]
        Planalto = 4314704,

        [EnumValue("4314779")]
        [Description("Pontao - RS")]
        Pontao = 4314779,

        [EnumValue("4314803")]
        [Description("Portao - RS")]
        Portao = 4314803,

        [EnumValue("4314902")]
        [Description("Porto Alegre - RS")]
        PortoAlegre = 4314902,

        [EnumValue("4315057")]
        [Description("Porto Maua - RS")]
        PortoMaua = 4315057,

        [EnumValue("4315107")]
        [Description("Porto Xavier - RS")]
        PortoXavier = 4315107,

        [EnumValue("4315404")]
        [Description("Redentora - RS")]
        Redentora = 4315404,

        [EnumValue("4315602")]
        [Description("Rio Grande - RS")]
        RioGrande = 4315602,

        [EnumValue("4315909")]
        [Description("Rodeio Bonito - RS")]
        RodeioBonito = 4315909,

        [EnumValue("4316006")]
        [Description("Rolante - RS")]
        Rolante = 4316006,

        [EnumValue("4316105")]
        [Description("Ronda Alta - RS")]
        RondaAlta = 4316105,

        [EnumValue("4316204")]
        [Description("Rondinha - RS")]
        Rondinha = 4316204,

        [EnumValue("4316600")]
        [Description("Sananduva - RS")]
        Sananduva = 4316600,

        [EnumValue("4316758")]
        [Description("Santa Clara do Sul - RS")]
        SantaClaradoSul = 4316758,

        [EnumValue("4316808")]
        [Description("Santa Cruz do Sul - RS")]
        SantaCruzdoSul = 4316808,

        [EnumValue("4316907")]
        [Description("Santa Maria - RS")]
        SantaMaria = 4316907,

        [EnumValue("4317103")]
        [Description("Santana do Livramento - RS")]
        SantanadoLivramento = 4317103,

        [EnumValue("4317202")]
        [Description("Santa Rosa - RS")]
        SantaRosa = 4317202,

        [EnumValue("4317400")]
        [Description("Santiago - RS")]
        Santiago = 4317400,

        [EnumValue("4317509")]
        [Description("Santo Angelo - RS")]
        SantoAngelo = 4317509,

        [EnumValue("4317608")]
        [Description("Santo Antonio da Patrulha - RS")]
        SantoAntoniodaPatrulha = 4317608,

        [EnumValue("4317707")]
        [Description("Santo Antonio das Missoes - RS")]
        SantoAntoniodasMissoes = 4317707,

        [EnumValue("4317756")]
        [Description("Santo Antonio do Planalto - RS")]
        SantoAntoniodoPlanalto = 4317756,

        [EnumValue("4317806")]
        [Description("Santo Augusto - RS")]
        SantoAugusto = 4317806,

        [EnumValue("4317905")]
        [Description("Santo Cristo - RS")]
        SantoCristo = 4317905,

        [EnumValue("4318002")]
        [Description("Santo Borja - RS")]
        SantoBorja = 4318002,

        [EnumValue("4318051")]
        [Description("Sao Domingos do Sul - RS")]
        SaoDomingosdoSul = 4318051,

        [EnumValue("4318200")]
        [Description("Sao Francisco de Paula - RS")]
        SaoFranciscodePaula = 4318200,

        [EnumValue("4318309")]
        [Description("Sao Gabriel - RS")]
        SaoGabriel = 4318309,

        [EnumValue("4318408")]
        [Description("Sao Jeronimo - RS")]
        SaoJeronimo = 4318408,

        [EnumValue("4318424")]
        [Description("Sao Joao da Urtiga - RS")]
        SaoJoaodaUrtiga = 4318424,

        [EnumValue("4318457")]
        [Description("Sao Jose das Missoes - RS")]
        SaoJosedasMissoes = 4318457,

        [EnumValue("4318606")]
        [Description("Sao Jose do Ouro - RS")]
        SaoJosedoOuro = 4318606,

        [EnumValue("4318705")]
        [Description("Sao Leopoldo - RS")]
        SaoLeopoldo = 4318705,

        [EnumValue("4318903")]
        [Description("Sao Luiz Gonzaga - RS")]
        SaoLuizGonzaga = 4318903,

        [EnumValue("4319158")]
        [Description("Sao Miguel das Missoes - RS")]
        SaoMigueldasMissoes = 4319158,

        [EnumValue("4319372")]
        [Description("Sao Pedro do Butia - RS")]
        SaoPedrodoButia = 4319372,

        [EnumValue("4319505")]
        [Description("Sao Sebastiao do Cai - RS")]
        SaoSebastiaodoCai = 4319505,

        [EnumValue("4319604")]
        [Description("Sao Sepe - RS")]
        SaoSepe = 4319604,

        [EnumValue("4319901")]
        [Description("Sapiranga - RS")]
        Sapiranga = 4319901,

        [EnumValue("4320008")]
        [Description("Sapucaia do Sul - RS")]
        SapucaiadoSul = 4320008,

        [EnumValue("4320107")]
        [Description("Sarandi - RS")]
        SarandiRS = 4320107,

        [EnumValue("4320206")]
        [Description("Seberi - RS")]
        Seberi = 4320206,

        [EnumValue("4320404")]
        [Description("Serafina Correa - RS")]
        SerafinaCorrea = 4320404,

        [EnumValue("4320552")]
        [Description("Sertao Santana - RS")]
        SertaoSantana = 4320552,

        [EnumValue("4320800")]
        [Description("Soledade - RS")]
        Soledade = 4320800,

        [EnumValue("4320909")]
        [Description("Tapejara - RS")]
        Tapejara = 4320909,

        [EnumValue("4321006")]
        [Description("Tapera - RS")]
        Tapera = 4321006,

        [EnumValue("4321204")]
        [Description("Taquara - RS")]
        Taquara = 4321204,

        [EnumValue("4321303")]
        [Description("Taquari - RS")]
        Taquari = 4321303,

        [EnumValue("4321402")]
        [Description("Tenente Portela - RS")]
        TenentePortela = 4321402,

        [EnumValue("4321436")]
        [Description("Terra de Areia - RS")]
        TerradeAreia = 4321436,

        [EnumValue("4321451")]
        [Description("Teutonia - RS")]
        Teutonia = 4321451,

        [EnumValue("4321469")]
        [Description("Tio Hugo - RS")]
        TioHugo = 4321469,

        [EnumValue("4321501")]
        [Description("Torres - RS")]
        Torres = 4321501,

        [EnumValue("4321600")]
        [Description("Tramandai - RS")]
        Tramandai = 4321600,

        [EnumValue("4321634")]
        [Description("Tres Arroios - RS")]
        TresArroios = 4321634,

        [EnumValue("4321667")]
        [Description("Tres Cachoeiras - RS")]
        TresCachoeiras = 4321667,

        [EnumValue("4321709")]
        [Description("Tres Coroas - RS")]
        TresCoroas = 4321709,

        [EnumValue("4321808")]
        [Description("Tres de Maio - RS")]
        TresdeMaio = 4321808,

        [EnumValue("4321907")]
        [Description("Tres Passos - RS")]
        TresPassos = 4321907,

        [EnumValue("4321956")]
        [Description("Trindade do Sul - RS")]
        TrindadedoSul = 4321956,

        [EnumValue("4322004")]
        [Description("Triunfo - RS")]
        Triunfo = 4322004,

        [EnumValue("4322103")]
        [Description("Tucunduva - RS")]
        Tucunduva = 4322103,

        [EnumValue("4322301")]
        [Description("Tuparendi - RS")]
        Tuparendi = 4322301,

        [EnumValue("4322400")]
        [Description("Uruguaiana - RS")]
        Uruguaiana = 4322400,

        [EnumValue("4322509")]
        [Description("Vacaria - RS")]
        Vacaria = 4322509,

        [EnumValue("4322558")]
        [Description("Vanini - RS")]
        Vanini = 4322558,

        [EnumValue("4322608")]
        [Description("Venancio Aires - RS")]
        VenancioAires = 4322608,

        [EnumValue("4322707")]
        [Description("Vera Cruz - RS")]
        VeraCruzRS = 4322707,

        [EnumValue("4322806")]
        [Description("Veranopolis - RS")]
        Veranopolis = 4322806,

        [EnumValue("4323002")]
        [Description("Viamao - RS")]
        Viamao = 4323002,

        [EnumValue("4323358")]
        [Description("Vila Langaro - RS")]
        VilaLangaro = 4323358,

        [EnumValue("4323507")]
        [Description("Vila Alegre - RS")]
        VistaAlegre = 4323507,

        [EnumValue("4323770")]
        [Description("Westfalia - RS")]
        Westfalia = 4323770,

        //Mato Grosso do Sul
        [EnumValue("5000708")]
        [Description("Anastacio - MS")]
        Anastacio = 5000708,

        [EnumValue("5001102")]
        [Description("Aquidauana - MS")]
        Aquidauana = 5001102,

        [EnumValue("5001243")]
        [Description("Aral Moreira - MS")]
        AralMoreira = 5001243,

        [EnumValue("5001904")]
        [Description("Bataguassu - MS")]
        Bataguassu = 5001904,

        [EnumValue("5002001")]
        [Description("Bataypora - MS")]
        Bataypora = 5002001,

        [EnumValue("5002100")]
        [Description("Bela Vista - MS")]
        BelaVista = 5002100,

        [EnumValue("5002159")]
        [Description("Bodoquena - MS")]
        Bodoquena = 5002159,

        [EnumValue("5002209")]
        [Description("Bonito - MS")]
        Bonito = 5002209,

        [EnumValue("5002308")]
        [Description("Brasilandia - MS")]
        Brasilandia = 5002308,

        [EnumValue("5002407")]
        [Description("Caarapo - MS")]
        Caarapo = 5002407,

        [EnumValue("5002704")]
        [Description("Campo Grande - MS")]
        CampoGrande = 5002704,

        [EnumValue("5003157")]
        [Description("Coronel Sapucaia - MS")]
        CoronelSapucaia = 5003157,

        [EnumValue("5003207")]
        [Description("Corumba - MS")]
        Corumba = 5003207,

        [EnumValue("5003454")]
        [Description("Deodapolis - MS")]
        Deodapolis = 5003454,

        [EnumValue("5003702")]
        [Description("Dourados - MS")]
        Dourados = 5003702,

        [EnumValue("5003801")]
        [Description("Fatima do Sul - MS")]
        FatimadoSul = 5003801,

        [EnumValue("5004106")]
        [Description("Guia Lopes da Laguna - MS")]
        GuiaLopesdaLaguna = 5004106,

        [EnumValue("5004304")]
        [Description("Iguatemi - MS")]
        Iguatemi = 5004304,

        [EnumValue("5004403")]
        [Description("Inocencia - MS")]
        Inocencia = 5004403,

        [EnumValue("5004601")]
        [Description("Itaquirai - MS")]
        Itaquirai = 5004601,

        [EnumValue("5004700")]
        [Description("Ivinhema - MS")]
        Ivinhema = 5004700,

        [EnumValue("5005004")]
        [Description("Jardim - MS")]
        Jardim = 5005004,

        [EnumValue("5005400")]
        [Description("Maracaju - MS")]
        Maracaju = 5005400,

        [EnumValue("5005608")]
        [Description("Miranda - MS")]
        Miranda = 5005608,

        [EnumValue("5005681")]
        [Description("Mundo Novo - MS")]
        MundoNovoMS = 5005681,

        [EnumValue("5005707")]
        [Description("Navirai - MS")]
        Navirai = 5005707,

        [EnumValue("5006002")]
        [Description("Nova Alvorada do Sul - MS")]
        NovaAlvoradadoSul = 5006002,

        [EnumValue("5006200")]
        [Description("Nova Andradina - MS")]
        NovaAndradina = 5006200,

        [EnumValue("5006259")]
        [Description("Novo Horizonte do Sul - MS")]
        NovoHorizontedoSul = 5006259,

        [EnumValue("5006309")]
        [Description("Paranaiba - MS")]
        Paranaiba = 5006309,

        [EnumValue("5006606")]
        [Description("Ponta Pora - MS")]
        PontaPora = 5006606,

        [EnumValue("5006903")]
        [Description("Porto Murtinho - MS")]
        PortoMurtinho = 5006903,

        [EnumValue("5007109")]
        [Description("Ribas do Rio Pardo - MS")]
        RibasdoRioPardo = 5007109,

        [EnumValue("5007208")]
        [Description("Rio Brilhante - MS")]
        RioBrilhante = 5007208,

        [EnumValue("5007406")]
        [Description("Rio Verde de Mato Grosso - MS")]
        RioVerdedeMatoGrosso = 5007406,

        [EnumValue("5007554")]
        [Description("Santa Rita do Pardo - MS")]
        SantaRitadoPardo = 5007554,

        [EnumValue("5007695")]
        [Description("Sao Gabriel do Oeste - MS")]
        SaoGabrieldoOeste = 5007695,

        [EnumValue("5007802")]
        [Description("Selviria - MS")]
        Selviria = 5007802,

        [EnumValue("5007901")]
        [Description("Sidrolandia - MS")]
        Sidrolandia = 5007901,

        [EnumValue("5007976")]
        [Description("Taquarussu - MS")]
        Taquarussu = 5007976,

        [EnumValue("5008305")]
        [Description("Tres Lagoas - MS")]
        TresLagoas = 5008305,

        //Mato Grosso
        [EnumValue("5100201")]
        [Description("Agua Boa - MT")]
        AguaBoa = 5100201,

        [EnumValue("5100250")]
        [Description("Alta Floresta - MT")]
        AltaFloresta = 5100250,

        [EnumValue("5100300")]
        [Description("Alto Araguaia - MT")]
        AltoAraguaia = 5100300,

        [EnumValue("5100409")]
        [Description("Alto Garcas - MT")]
        AltoGarcas = 5100409,

        [EnumValue("5100805")]
        [Description("Apiacas - MT")]
        Apiacas = 5100805,

        [EnumValue("5101308")]
        [Description("Arenapolis - MT")]
        Arenapolis = 5101308,

        [EnumValue("5101803")]
        [Description("Barra do Garcas - MT")]
        BarradoGarcas = 5101803,

        [EnumValue("5101902")]
        [Description("Brasnorte - MT")]
        Brasnorte = 5101902,

        [EnumValue("5102504")]
        [Description("Caceres - MT")]
        Caceres = 5102504,

        [EnumValue("5102637")]
        [Description("Campo Novo do Parecis - MT")]
        CampoNovodoParecis = 5102637,

        [EnumValue("5102678")]
        [Description("Campo Verde - MT")]
        CampoVerde = 5102678,

        [EnumValue("5102686")]
        [Description("Campo de Julio - MT")]
        CampodeJulio = 5102686,

        [EnumValue("5102702")]
        [Description("Canarana - MT")]
        Canarana = 5102702,

        [EnumValue("5103007")]
        [Description("Chapada dos Guimaraes - MT")]
        ChapadadosGuimaraes = 5103007,

        [EnumValue("5103205")]
        [Description("Colider - MT")]
        Colider = 5103205,

        [EnumValue("5103254")]
        [Description("Colniza - MT")]
        Colniza = 5103254,

        [EnumValue("5103304")]
        [Description("Comodoro - MT")]
        Comodoro = 5103304,

        [EnumValue("5103353")]
        [Description("Confresa - MT")]
        Confresa = 5103353,

        [EnumValue("5103361")]
        [Description("Conquista do Oeste - MT")]
        ConquistadoOeste = 5103361,

        [EnumValue("5103379")]
        [Description("Cotriguacu - MT")]
        Cotriguacu = 5103379,

        [EnumValue("5103403")]
        [Description("Cuiaba - MT")]
        Cuiaba = 5103403,

        [EnumValue("5103452")]
        [Description("Denise - MT")]
        Denise = 5103452,

        [EnumValue("5103502")]
        [Description("Diamantino - MT")]
        Diamantino = 5103502,

        [EnumValue("5103700")]
        [Description("Feliz Natal - MT")]
        FelizNatal = 5103700,

        [EnumValue("5104104")]
        [Description("Guaranta do Norte - MT")]
        GuarantadoNorte = 5104104,

        [EnumValue("5104526")]
        [Description("Ipiranga do Norte - MT")]
        IpirangadoNorte = 5104526,

        [EnumValue("5104542")]
        [Description("Itanhanga - MT")]
        Itanhanga = 5104542,

        [EnumValue("5104559")]
        [Description("Itauba - MT")]
        Itauba = 5104559,

        [EnumValue("5104609")]
        [Description("Itiquira - MT")]
        Itiquira = 5104609,

        [EnumValue("5104807")]
        [Description("Jaciara - MT")]
        Jaciara = 5104807,

        [EnumValue("5105101")]
        [Description("Juara - MT")]
        Juara = 5105101,

        [EnumValue("5105150")]
        [Description("Juina - MT")]
        Juina = 5105150,

        [EnumValue("5105259")]
        [Description("Lucas do Rio Verde - MT")]
        LucasdoRioVerde = 5105259,

        [EnumValue("5105606")]
        [Description("Matupa - MT")]
        Matupa = 5105606,

        [EnumValue("5105622")]
        [Description("Mirassol D Oeste - MT")]
        MirassolDOeste = 5105622,

        [EnumValue("5105903")]
        [Description("Nobres - MT")]
        Nobres = 5105903,

        [EnumValue("5106208")]
        [Description("Nova Brasilandia - MT")]
        NovaBrasilandia = 5106208,

        [EnumValue("5106224")]
        [Description("Nova Mutum - MT")]
        NovaMutum = 5106224,

        [EnumValue("5106232")]
        [Description("Nova Olimpia - MT")]
        NovaOlimpia = 5106232,

        [EnumValue("5106257")]
        [Description("Nova Xavantina - MT")]
        NovaXavantina = 5106257,

        [EnumValue("5106307")]
        [Description("Paranatinga - MT")]
        Paranatinga = 5106307,

        [EnumValue("5106372")]
        [Description("Pedra Preta - MT")]
        PedraPreta = 5106372,

        [EnumValue("5106422")]
        [Description("Peixoto de Azevedo - MT")]
        PeixotodeAzevedo = 5106422,

        [EnumValue("5106455")]
        [Description("Planalto da Serra - MT")]
        PlanaltodaSerra = 5106455,

        [EnumValue("5106505")]
        [Description("Pocone - MT")]
        Pocone = 5106505,

        [EnumValue("5106752")]
        [Description("Pontes E Lacerda - MT")]
        PontesELacerda = 5106752,

        [EnumValue("5106778")]
        [Description("Porto Alegre do Norte - MT")]
        PortoAlegredoNorte = 5106778,

        [EnumValue("5107008")]
        [Description("Poxoreo - MT")]
        Poxoreo = 5107008,

        [EnumValue("5107040")]
        [Description("Primavera do Leste - MT")]
        PrimaveradoLeste = 5107040,

        [EnumValue("5107065")]
        [Description("Querencia - MT")]
        Querencia = 5107065,

        [EnumValue("5107107")]
        [Description("Sao Jose Dos Quatro Marcos - MT")]
        SaoJoseDosQuatroMarcos = 5107107,

        [EnumValue("5107180")]
        [Description("Ribeirao Cascalheira - MT")]
        RibeiraoCascalheira = 5107180,

        [EnumValue("5107248")]
        [Description("Santa Carmen - MT")]
        SantaCarmen = 5107248,

        [EnumValue("5107305")]
        [Description("Sao Jose do Rio Claro - MT")]
        SaoJosedoRioClaro = 5107305,

        [EnumValue("5107602")]
        [Description("Rondonopolis - MT")]
        Rondonopolis = 5107602,

        [EnumValue("5107768")]
        [Description("Santa Rita do Trivelato - MT")]
        SantaRitadoTrivelato = 5107768,

        [EnumValue("5107800")]
        [Description("Santo Antonio do Leverger - MT")]
        SantoAntoniodoLeverger = 5107800,

        [EnumValue("5107859")]
        [Description("Sao Felix do Araguaia - MT")]
        SaoFelixdoAraguaia = 5107859,

        [EnumValue("5107875")]
        [Description("Sapezal - MT")]
        Sapezal = 5107875,

        [EnumValue("5107909")]
        [Description("Sinop - MT")]
        Sinop = 5107909,

        [EnumValue("5107925")]
        [Description("Sorriso - MT")]
        Sorriso = 5107925,

        [EnumValue("5107958")]
        [Description("Tangara da Serra - MT")]
        TangaradaSerra = 5107958,

        [EnumValue("5108006")]
        [Description("Tapurah - MT")]
        Tapurah = 5108006,

        [EnumValue("5108204")]
        [Description("Torixoreu - MT")]
        Torixoreu = 5108204,

        [EnumValue("5108352")]
        [Description("Vale de Sao Domingos - MT")]
        ValedeSaoDomingos = 5108352,

        [EnumValue("5108402")]
        [Description("Varzea Grande - MT")]
        VarzeaGrande = 5108402,

        [EnumValue("5108600")]
        [Description("Vila Rica - MT")]
        VilaRica = 5108600,

        [EnumValue("5108907")]
        [Description("Nova Maringa - MT")]
        NovaMaringa = 5108907,

        //Goiás
        [EnumValue("5200134")]
        [Description("Acreuna - GO")]
        Acreuna = 5200134,

        [EnumValue("5200159")]
        [Description("Adelandia - GO")]
        Adelandia = 5200159,

        [EnumValue("5200209")]
        [Description("Agua Limpa - GO")]
        AguaLimpa = 5200209,

        [EnumValue("5200258")]
        [Description("Aguas Lindas de Goias - GO")]
        AguasLindasdeGoias = 5200258,

        [EnumValue("5201108")]
        [Description("Anapolis - GO")]
        Anapolis = 5201108,

        [EnumValue("5201405")]
        [Description("Aparecida de Goiania - GO")]
        AparecidadeGoiania = 5201405,

        [EnumValue("5201454")]
        [Description("Aparecida do Rio Doce - GO")]
        AparecidadoRioDoce = 5201454,

        [EnumValue("5201504")]
        [Description("Apore - GO")]
        Apore = 5201504,

        [EnumValue("5201603")]
        [Description("Aracu - GO")]
        Aracu = 5201603,

        [EnumValue("5201702")]
        [Description("Aragarcas - GO")]
        Aragarcas = 5201702,

        [EnumValue("5201801")]
        [Description("Aragoiania - GO")]
        Aragoiania = 5201801,

        [EnumValue("5203609")]
        [Description("Brazabrantes - GO")]
        Brazabrantes = 5203609,

        [EnumValue("5204003")]
        [Description("Cabeceiras - GO")]
        Cabeceiras = 5204003,

        [EnumValue("5204300")]
        [Description("Cacu - GO")]
        Cacu = 5204300,

        [EnumValue("5204409")]
        [Description("Caiaponia - GO")]
        Caiaponia = 5204409,

        [EnumValue("5204508")]
        [Description("Caldas Novas - GO")]
        CaldasNovas = 5204508,

        [EnumValue("5204607")]
        [Description("Campestre de Goias - GO")]
        CampestredeGoias = 5204607,

        [EnumValue("5205000")]
        [Description("Carmo do Rio Verde - GO")]
        CarmodoRioVerde = 5205000,

        [EnumValue("5205109")]
        [Description("Catalao - GO")]
        Catalao = 5205109,

        [EnumValue("5205406")]
        [Description("Ceres - GO")]
        Ceres = 5205406,

        [EnumValue("5205455")]
        [Description("Cezarina - GO")]
        Cezarina = 5205455,

        [EnumValue("5205471")]
        [Description("Chapadao do Ceu - GO")]
        ChapadaodoCeu = 5205471,

        [EnumValue("5205802")]
        [Description("Corumbá de Goiás - GO")]
        CorumbádeGoiás = 5205802,

        [EnumValue("5206404")]
        [Description("Crixas - GO")]
        Crixas = 5206404,

        [EnumValue("5207402")]
        [Description("Edeia - GO")]
        Edeia = 5207402,

        [EnumValue("5207535")]
        [Description("Faina - GO")]
        Faina = 5207535,

        [EnumValue("5207907")]
        [Description("Flores de Goias - GO")]
        FloresdeGoias = 5207907,

        [EnumValue("5208004")]
        [Description("Formosa - GO")]
        Formosa = 5208004,

        [EnumValue("5208608")]
        [Description("Goianesia - GO")]
        Goianesia = 5208608,

        [EnumValue("5208707")]
        [Description("Goiania - GO")]
        Goiania = 5208707,

        [EnumValue("5208806")]
        [Description("Goianira - GO")]
        Goianira = 5208806,

        [EnumValue("5208905")]
        [Description("Goias - GO")]
        Goias = 5208905,

        [EnumValue("5209291")]
        [Description("Guaraita - GO")]
        Guaraita = 5209291,

        [EnumValue("5209606")]
        [Description("Heitorai - GO")]
        Heitorai = 5209606,

        [EnumValue("5209952")]
        [Description("Indiara - GO")]
        Indiara = 5209952,

        [EnumValue("5210000")]
        [Description("Inhumas - GO")]
        Inhumas = 5210000,

        [EnumValue("5210208")]
        [Description("Ipora - GO")]
        IporaGO = 5210208,

        [EnumValue("5210406")]
        [Description("Itaberai - GO")]
        Itaberai = 5210406,

        [EnumValue("5210802")]
        [Description("Itaja - GO")]
        Itaja = 5210802,

        [EnumValue("5211008")]
        [Description("Itapirapua - GO")]
        Itapirapua = 5211008,

        [EnumValue("5211305")]
        [Description("Itaruma - GO")]
        Itaruma = 5211305,

        [EnumValue("5211503")]
        [Description("Itumbiara - GO")]
        Itumbiara = 5211503,

        [EnumValue("5211701")]
        [Description("Jandaia - GO")]
        Jandaia = 5211701,

        [EnumValue("5211909")]
        [Description("Jatai - GO")]
        Jatai = 5211909,

        [EnumValue("5212253")]
        [Description("Lagoa Santa - GO")]
        LagoaSantaGO = 5212253,

        [EnumValue("5212501")]
        [Description("Luziania - GO")]
        Luziania = 5212501,

        [EnumValue("5212709")]
        [Description("Mambai - GO")]
        Mambai = 5212709,

        [EnumValue("5212907")]
        [Description("Marzagao - GO")]
        Marzagao = 5212907,

        [EnumValue("5213103")]
        [Description("Mineiros - GO")]
        Mineiros = 5213103,

        [EnumValue("5213509")]
        [Description("Monte Alegre de Goias - GO")]
        MonteAlegredeGoias = 5213509,

        [EnumValue("5213756")]
        [Description("Montividiu - GO")]
        Montividiu = 5213756,

        [EnumValue("5214002")]
        [Description("Mozarlandia - GO")]
        Mozarlandia = 5214002,

        [EnumValue("5214051")]
        [Description("Mundo Novo - GO")]
        MundoNovoGO = 5214051,

        [EnumValue("5214507")]
        [Description("Neropolis - GO")]
        Neropolis = 5214507,

        [EnumValue("5214838")]
        [Description("Nova Crixas - GO")]
        NovaCrixas = 5214838,

        [EnumValue("5215009")]
        [Description("Nova Veneza - GO")]
        NovaVenezaGO = 5215009,

        [EnumValue("5215306")]
        [Description("Orizona - GO")]
        Orizona = 5215306,

        [EnumValue("5215405")]
        [Description("Ouro Verde de Goias - GO")]
        OuroVerdedeGoias = 5215405,

        [EnumValue("5216304")]
        [Description("Paranaiguara - GO")]
        Paranaiguara = 5216304,

        [EnumValue("5216403")]
        [Description("Parauna - GO")]
        Parauna = 5216403,

        [EnumValue("5217104")]
        [Description("Piracanjuba - GO")]
        Piracanjuba = 5217104,

        [EnumValue("5217302")]
        [Description("Pirenopolis - GO")]
        Pirenopolis = 5217302,

        [EnumValue("5217401")]
        [Description("Pires do Rio - GO")]
        PiresdoRio = 5217401,

        [EnumValue("5217609")]
        [Description("Planaltina - GO")]
        Planaltina = 5217609,

        [EnumValue("5217708")]
        [Description("Pontalina - GO")]
        Pontalina = 5217708,

        [EnumValue("5218003")]
        [Description("Porangatu - GO")]
        Porangatu = 5218003,

        [EnumValue("5218300")]
        [Description("Posse - GO")]
        Posse = 5218300,

        [EnumValue("5218805")]
        [Description("Rio Verde - GO")]
        RioVerde = 5218805,

        [EnumValue("5218904")]
        [Description("Rubiataba - GO")]
        Rubiataba = 5218904,

        [EnumValue("5219258")]
        [Description("Santa Fe de Goias - GO")]
        SantaFedeGoias = 5219258,

        [EnumValue("5220108")]
        [Description("Sao Luis de Montes Belos - GO")]
        SaoLuisdeMontesBelos = 5220108,

        [EnumValue("5220207")]
        [Description("Sao Miguel do Araguaia - GO")]
        SaoMigueldoAraguaia = 5220207,

        [EnumValue("5220454")]
        [Description("Senador Canedo - GO")]
        SenadorCanedo = 5220454,

        [EnumValue("5220603")]
        [Description("Silvania - GO")]
        Silvania = 5220603,

        [EnumValue("5221197")]
        [Description("Terezopolis de Goias - GO")]
        TerezopolisdeGoias = 5221197,

        [EnumValue("5221403")]
        [Description("Trindade - GO")]
        Trindade = 5221403,

        [EnumValue("5221601")]
        [Description("Uruacu - GO")]
        Uruacu = 5221601,

        [EnumValue("5221858")]
        [Description("Valparaiso de Goias - GO")]
        ValparaisodeGoias = 5221858,

        //Distrito Federal
        [EnumValue("5300108")]
        [Description("Brasilia - DF")]
        Brasilia = 5300108,
    }
}