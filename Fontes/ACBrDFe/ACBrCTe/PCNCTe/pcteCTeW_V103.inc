////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              Gera o XML para a versão 1.03                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  TGeradorOpcoes = class;

  TCTeW = class(TPersistent)
  private
    FGerador: TGerador;
    FCTe: TCTe;
    FOpcoes: TGeradorOpcoes;

    procedure GerarInfCTe;     // Nivel 0

    procedure GerarIde;        // Nivel 1
    procedure GerarToma03;     // Nivel 2
    procedure GerarToma4;      // Nivel 2
    procedure GerarEnderToma;  // Nivel 3

    procedure GerarCompl;      // Nivel 1
    procedure GerarFluxo;      // Nivel 2
    procedure GerarEntrega;    // Nivel 2
    procedure GerarObsCont;    // Nivel 2
    procedure GerarObsFisco;   // Nivel 2

    procedure GerarEmit;       // Nivel 1
    procedure GerarEnderEmit;  // Nivel 2

    procedure GerarRem;        // Nivel 1
    procedure GerarEnderReme;  // Nivel 2
    procedure GerarInfNF;      // Nivel 2
    procedure GerarInfNFE;     // Nivel 2
    procedure GerarInfOutros;  // Nivel 2

    procedure GerarExped;      // Nivel 1
    procedure GerarEnderExped; // Nivel 2

    procedure GerarReceb;      // Nivel 1
    procedure GerarEnderReceb; // Nivel 2

    procedure GerarDest;       // Nivel 1
    procedure GerarEnderDest;  // Nivel 2
    procedure GerarLocEnt;     // Nivel 2

    procedure GerarVPrest;     // Nivel 1
    procedure GerarComp;       // Nivel 2

    procedure GerarImp;        // Nivel 1
    procedure GerarICMS;       // Nivel 2
    procedure GerarCST00;      // Nivel 3
    procedure GerarCST20;      // Nivel 3
    procedure GerarCST45;      // Nivel 3
    procedure GerarCST80;      // Nivel 3
    procedure GerarCST81;      // Nivel 3
    procedure GerarCST90;      // Nivel 3

    procedure GerarInfCTeNorm; // Nivel 1
    procedure GerarinfCarga;   // Nivel 2
    procedure GerarInfQ;       // Nivel 3
    procedure GerarContQt;     // Nivel 2
    procedure GerarDocAnt;     // Nivel 2
    procedure GerarInfSeg;     // Nivel 2
    procedure GerarRodo;       // Nivel 2
    procedure GerarCTRB;       // Nivel 3
    procedure GerarOCC;        // Nivel 3
    procedure GerarValePed;    // Nivel 3
    procedure GerarVeic;       // Nivel 3
    procedure GerarLacre;      // Nivel 3
    procedure GerarMoto;       // Nivel 3
    procedure GerarAereo;      // Nivel 2
    procedure GerarAquav;      // Nivel 2
    procedure GerarFerrov;     // Nivel 2
    procedure GerarFerroSub;   // Nivel 3
    procedure GerarEnderFerro; // Nivel 4
    procedure GerarDCL;        // Nivel 3
    procedure GerardetVag;     // Nivel 3

    procedure GerarDuto;       // Nivel 2
    procedure GerarPeri;       // Nivel 2
    procedure GerarVeicNovos;  // Nivel 2
    procedure GerarCobr;       // Nivel 2
    procedure GerarCobrFat;
    procedure GerarCobrDup;
    procedure GerarInfCTeSub;  // Nivel 2

    procedure GerarInfCTeComp; // Nivel 1
    procedure GerarImpComp(i: Integer);    // Nivel 2
    procedure GerarICMSComp(i: Integer);   // Nivel 3
    procedure GerarCST00Comp(i: Integer);  // Nivel 4
    procedure GerarCST20Comp(i: Integer);  // Nivel 4
    procedure GerarCST45Comp(i: Integer);  // Nivel 4
    procedure GerarCST80Comp(i: Integer);  // Nivel 4
    procedure GerarCST81Comp(i: Integer);  // Nivel 4
    procedure GerarCST90Comp(i: Integer);  // Nivel 4

    procedure GerarInfCTeAnu;  // Nivel 1

    procedure AjustarMunicipioUF(var xUF: string; var xMun: string; var cMun: integer; cPais: integer; vxUF, vxMun: string; vcMun: integer);
    function ObterNomeMunicipio(const xMun, xUF: string; const cMun: integer): string;

  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
    function GerarXml: boolean;
    function ObterNomeArquivo: string;
  published
    property Gerador: TGerador read FGerador write FGerador;
    property CTe: TCTe read FCTe write FCTe;
    property Opcoes: TGeradorOpcoes read FOpcoes write FOpcoes;
  end;

  TGeradorOpcoes = class(TPersistent)
  private
    FAjustarTagNro: boolean;
    FNormatizarMunicipios: boolean;
    FGerarTagAssinatura: TpcnTagAssinatura;
    FPathArquivoMunicipios: string;
    FValidarInscricoes: boolean;
    FValidarListaServicos: boolean;
  published
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios;
    property GerarTagAssinatura: TpcnTagAssinatura read FGerarTagAssinatura write FGerarTagAssinatura;
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: boolean read FValidarInscricoes write FValidarInscricoes;
    property ValidarListaServicos: boolean read FValidarListaServicos write FValidarListaServicos;
  end;

  ////////////////////////////////////////////////////////////////////////////////

implementation

// Regra a ser aplicada em ambiente de homologação a partir de 01/09/2012
const
 xRazao = 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';

{ TCTeW }

constructor TCTeW.Create(AOwner: TCTe);
begin
  FCTe := AOwner;
  FGerador := TGerador.Create;
  FGerador.FIgnorarTagNivel := '|?xml version|CTe xmlns|infCTe versao|obsCont|obsFisco|';
  FOpcoes := TGeradorOpcoes.Create;
  FOpcoes.FAjustarTagNro := True;
  FOpcoes.FNormatizarMunicipios := False;
  FOpcoes.FGerarTagAssinatura := taSomenteSeAssinada;
  FOpcoes.FValidarInscricoes := False;
  FOpcoes.FValidarListaServicos := False;
end;

destructor TCTeW.Destroy;
begin
  FGerador.Free;
  FOpcoes.Free;
  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////

function TCTeW.ObterNomeArquivo: string;
begin
  Result := SomenteNumeros(CTe.infCTe.ID) + '-cte.xml';
end;

function TCTeW.GerarXml: boolean;
var
  chave: AnsiString;
  Gerar: boolean;
  xProtCTe : String;
begin
  chave := '';

  if not GerarChaveCTe(Chave, CTe.ide.cUF, CTe.ide.cCT, StrToInt(CTe.ide.modelo), CTe.ide.serie,
    CTe.ide.nCT, CTe.ide.dhEmi, CTe.Emit.CNPJ) then
    Gerador.wAlerta('#001', 'infCte', DSC_CHAVE, ERR_MSG_GERAR_CHAVE);

  chave := StringReplace(chave,'NFe','CTe',[rfReplaceAll]);
  CTe.infCTe.ID := chave;
  CTe.ide.cDV := RetornarDigito(CTe.infCTe.ID);
  CTe.Ide.cCT := RetornarCodigoNumericoCTe(CTe.infCTe.ID);

  // Carrega Layout que sera utilizado para gera o txt
  Gerador.LayoutArquivoTXT.Clear;
  Gerador.ArquivoFormatoXML := '';
  Gerador.ArquivoFormatoTXT := '';

  Gerador.wGrupo(ENCODING_UTF8, '', False);

  if CTe.procCTe.nProt <> ''
   then Gerador.wGrupo('cteProc versao="' + CTeenviCTe + '" ' + NAME_SPACE_CTE, '');
  Gerador.wGrupo('CTe ' + NAME_SPACE_CTE);
  Gerador.wGrupo('infCte versao="' + CTeenviCTe + '" Id="' + CTe.infCTe.ID + '"');

  (**)GerarInfCTe;
  Gerador.wGrupo('/infCte');
  //
  if FOpcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := true;
    if FOpcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((CTe.signature.DigestValue <> '') and (CTe.signature.SignatureValue <> '') and (CTe.signature.X509Certificate <> ''));
    if FOpcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
      Gerar := ((CTe.signature.DigestValue = '') and (CTe.signature.SignatureValue = '') and (CTe.signature.X509Certificate = ''));
    if Gerar then
    begin
      FCTe.signature.URI := somenteNumeros(CTe.infCTe.ID);
      FCTe.signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      FCTe.signature.GerarXMLCTe;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + FCTe.signature.Gerador.ArquivoFormatoXML;
    end;
  end;
  Gerador.wGrupo('/CTe');

  if CTe.procCTe.nProt <> '' then
   begin
     xProtCTe :=
           '<protCTe versao="' + CTeenviCTe + '">' +
             '<infProt>'+
               '<tpAmb>'+TpAmbToStr(CTe.procCTe.tpAmb)+'</tpAmb>'+
               '<verAplic>'+CTe.procCTe.verAplic+'</verAplic>'+
               '<chCTe>'+CTe.procCTe.chCTe+'</chCTe>'+
               '<dhRecbto>'+FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',CTe.procCTe.dhRecbto)+'</dhRecbto>'+
               '<nProt>'+CTe.procCTe.nProt+'</nProt>'+
               '<digVal>'+CTe.procCTe.digVal+'</digVal>'+
               '<cStat>'+IntToStr(CTe.procCTe.cStat)+'</cStat>'+
               '<xMotivo>'+CTe.procCTe.xMotivo+'</xMotivo>'+
             '</infProt>'+
           '</protCTe>';

     Gerador.wTexto(xProtCTe);
     Gerador.wGrupo('/cteProc');
   end;

  Gerador.gtAjustarRegistros(CTe.infCTe.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TCTeW.GerarInfCTe;
begin
  GerarIde;
  GerarCompl;
  GerarEmit;
  GerarRem;
  GerarExped;
  GerarReceb;
  GerarDest;
  GerarvPrest;
  GerarImp;

  GerarInfCTeNorm; // Gerado somente se Tipo de CTe = tcNormal
  GerarinfCTeComp; // Gerado somente se Tipo de CTe = tcComplemento
  GerarInfCTeAnu;  // Gerado somente se Tipo de CTe = tcAnulacao
end;

procedure TCTeW.GerarIde;
begin
  Gerador.wGrupo('ide', '#004');
  Gerador.wCampo(tcInt, '#005', 'cUF     ', 02, 02, 1, CTe.ide.cUF, DSC_CUF);
  if not ValidarCodigoUF(CTe.ide.cUF) then
    Gerador.wAlerta('#005', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, '#006', 'cCT     ', 09, 09, 1, IntToStrZero(RetornarCodigoNumericoCTe(CTe.infCTe.ID), 9), DSC_CNF);
  Gerador.wCampo(tcInt, '#007', 'CFOP    ', 04, 04, 1, CTe.ide.CFOP, DSC_CFOP);
  Gerador.wCampo(tcStr, '#008', 'natOp   ', 01, 60, 1, CTe.ide.natOp, DSC_NATOP);
  Gerador.wCampo(tcStr, '#009', 'forPag  ', 01, 01, 1, tpforPagToStr(CTe.ide.forPag), DSC_INDPAG);
  Gerador.wCampo(tcInt, '#010', 'mod     ', 02, 02, 1, CTe.ide.modelo, DSC_MOD);
  Gerador.wCampo(tcInt, '#011', 'serie   ', 01, 03, 1, CTe.ide.serie, DSC_SERIE);
  Gerador.wCampo(tcInt, '#012', 'nCT     ', 01, 09, 1, CTe.ide.nCT, DSC_NNF);
  Gerador.wCampo(tcDatHor, '#013', 'dhEmi', 19, 19, 1, CTe.ide.dhEmi, DSC_DEMI);
  Gerador.wCampo(tcStr, '#014', 'tpImp   ', 01, 01, 1, tpImpToStr(CTe.Ide.tpImp), DSC_TPIMP);
  Gerador.wCampo(tcStr, '#015', 'tpEmis  ', 01, 01, 1, tpEmisToStr(CTe.Ide.tpEmis), DSC_TPEMIS);
  Gerador.wCampo(tcInt, '#016', 'cDV     ', 01, 01, 1, CTe.Ide.cDV, DSC_CDV);
  Gerador.wCampo(tcStr, '#017', 'tpAmb   ', 01, 01, 1, tpAmbToStr(CTe.Ide.tpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, '#018', 'tpCTe   ', 01, 01, 1, tpCTePagToStr(CTe.Ide.tpCTe), DSC_TPCTE);
  Gerador.wCampo(tcStr, '#019', 'procEmi', 01, 01, 1, procEmiToStr(CTe.Ide.procEmi), DSC_PROCEMI);
  Gerador.wCampo(tcStr, '#020', 'verProc', 01, 20, 1, CTe.Ide.verProc, DSC_VERPROC);
  Gerador.wCampo(tcStr, '#021', 'refCTE ', 44, 44, 0, SomenteNumeros(CTe.Ide.refCTE), DSC_REFCTE);
  if SomenteNumeros(CTe.Ide.refCTe) <> '' then
    if not ValidarChave('NFe' + SomenteNumeros(CTe.Ide.refCTe)) then
      Gerador.wAlerta('#021', 'refCTE', DSC_REFCTE, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcInt, '#022', 'cMunEmi ', 07, 07, 1, CTe.ide.cMunEmi, DSC_CMUNEMI);
  if not ValidarMunicipio(CTe.ide.cMunEmi) then
    Gerador.wAlerta('#022', 'cMunEmi', DSC_CMUNEMI, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#023', 'xMunEmi ', 01, 60, 1, CTe.ide.xMunEmi, DSC_XMUN);
  Gerador.wCampo(tcStr, '#024', 'UFEmi   ', 02, 02, 1, CTe.ide.UFEmi, DSC_UF);
  if not ValidarUF(CTe.ide.UFEmi) then
    Gerador.wAlerta('#024', 'UFEmi', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#025', 'modal   ', 02, 02, 1, TpModalToStr(CTe.Ide.modal), DSC_MODAL);
  Gerador.wCampo(tcStr, '#026', 'tpServ  ', 01, 01, 1, TpServPagToStr(CTe.Ide.tpServ), DSC_TPSERV);
  Gerador.wCampo(tcInt, '#027', 'cMunIni ', 07, 07, 1, CTe.ide.cMunIni, DSC_CMUNEMI);
  if not ValidarMunicipio(CTe.ide.cMunIni) then
    Gerador.wAlerta('#027', 'cMunIni', DSC_CMUNEMI, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#028', 'xMunIni ', 01, 60, 1, CTe.ide.xMunIni, DSC_XMUN);
  Gerador.wCampo(tcStr, '#029', 'UFIni   ', 02, 02, 1, CTe.ide.UFIni, DSC_UF);
  if not ValidarUF(CTe.ide.UFIni) then
    Gerador.wAlerta('#029', 'UFIni', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcInt, '#030', 'cMunFim ', 07, 07, 1, CTe.ide.cMunFim, DSC_CMUNEMI);
  if not ValidarMunicipio(CTe.ide.cMunFim) then
    Gerador.wAlerta('#030', 'cMunFim', DSC_CMUNEMI, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#031', 'xMunFim    ', 01, 60, 1, CTe.ide.xMunFim, DSC_XMUN);
  Gerador.wCampo(tcStr, '#032', 'UFFim      ', 02, 02, 1, CTe.ide.UFFim, DSC_UF);
  if not ValidarUF(CTe.ide.UFFim) then
    Gerador.wAlerta('#032', 'UFFim', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#033', 'retira     ', 01, 01, 1, TpRetiraPagToStr(CTe.Ide.retira), DSC_RETIRA);
  Gerador.wCampo(tcStr, '#034', 'xDetRetira ', 01, 160, 0, CTe.Ide.xdetretira, DSC_DRET);

  (**)GerarToma03;
  (**)GerarToma4;

  Gerador.wGrupo('/ide');
end;

procedure TCTeW.GerarToma03;
begin
  if (CTe.Ide.Toma4.xNome = '') then
  begin
    Gerador.wGrupo('toma03', '#035');
    Gerador.wCampo(tcStr, '#036', 'toma ', 01, 01, 1, TpTomadorToStr(CTe.ide.Toma03.Toma), DSC_TOMA);
    Gerador.wGrupo('/toma03');
  end;
end;

procedure TCTeW.GerarToma4;
begin
  if (CTe.Ide.Toma4.IE <> '') or
     (CTe.Ide.Toma4.xNome <> '') then
  begin
    Gerador.wGrupo('toma4', '#037');
    Gerador.wCampo(tcStr, '#038', 'toma ', 01, 01, 1, TpTomadorToStr(CTe.ide.Toma4.Toma), DSC_TOMA);
    Gerador.wCampoCNPJCPF('#039', '#040', CTe.ide.Toma4.CNPJCPF, CTe.Ide.Toma4.EnderToma.cPais);

    if CTe.Ide.Toma4.IE <> ''
     then begin
      if Trim(CTe.Ide.Toma4.IE) = 'ISENTO'
       then Gerador.wCampo(tcStr, '#041', 'IE ', 00, 14, 1, CTe.Ide.Toma4.IE, DSC_IE)
       else Gerador.wCampo(tcStr, '#041', 'IE ', 00, 14, 1, SomenteNumeros(CTe.Ide.Toma4.IE), DSC_IE);

      if (FOpcoes.ValidarInscricoes)
       then if not ValidarIE(CTe.Ide.Toma4.IE, CTe.Ide.Toma4.EnderToma.UF) then
        Gerador.wAlerta('#041', 'IE', DSC_IE, ERR_MSG_INVALIDO);
     end;

    Gerador.wCampo(tcStr, '#042', 'xNome  ', 01, 60, 1, CTe.Ide.Toma4.xNome, DSC_XNOME);
    Gerador.wCampo(tcStr, '#043', 'xFant  ', 01, 60, 0, CTe.Ide.Toma4.xFant, DSC_XFANT);
    Gerador.wCampo(tcStr, '#044', 'fone  ', 07, 12, 0, CTe.Ide.Toma4.fone, DSC_FONE);

    (***)GerarEnderToma;

    Gerador.wGrupo('/toma4');
  end;
end;

procedure TCTeW.GerarEnderToma;
var
  cMun: integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CTe.Ide.Toma4.EnderToma.cPais,
                                      CTe.Ide.Toma4.EnderToma.UF,
                                      CTe.Ide.Toma4.EnderToma.xMun,
                                      CTe.Ide.Toma4.EnderToma.cMun);
  Gerador.wGrupo('enderToma', '#045');
  Gerador.wCampo(tcStr, '#046', 'xLgr   ', 01, 255, 1, CTe.Ide.Toma4.EnderToma.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#047', 'nro    ', 01, 60, 1, ExecutarAjusteTagNro(FOpcoes.FAjustarTagNro, CTe.Ide.Toma4.EnderToma.nro), DSC_NRO);
  Gerador.wCampo(tcStr, '#048', 'xCpl   ', 01, 60, 0, CTe.Ide.Toma4.EnderToma.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#049', 'xBairro', 01, 60, 1, CTe.Ide.Toma4.EnderToma.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#050', 'cMun   ', 07, 07, 1, cMun, DSC_CMUN);
  if not ValidarMunicipio(CTe.Ide.Toma4.EnderToma.cMun) then
    Gerador.wAlerta('#050', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#051', 'xMun   ', 01, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#052', 'CEP    ', 08, 08, 0, CTe.Ide.Toma4.EnderToma.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#053', 'UF     ', 02, 02, 1, xUF, DSC_UF);
  if not ValidarUF(xUF) then
    Gerador.wAlerta('#053', 'UF', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcInt, '#054', 'cPais  ', 04, 04, 0, CTe.Ide.Toma4.EnderToma.cPais, DSC_CPAIS); // Conforme NT-2009/01
  Gerador.wCampo(tcStr, '#055', 'xPais  ', 01, 60, 0, CTe.Ide.Toma4.EnderToma.xPais, DSC_XPAIS);
  Gerador.wGrupo('/enderToma');
end;

procedure TCTeW.GerarCompl;
begin
  Gerador.wGrupo('compl', '#059');//compl
  Gerador.wCampo(tcStr, '#060', 'xCaracAd  ', 01, 15, 0, CTe.Compl.xCaracAd, '');
  Gerador.wCampo(tcStr, '#061', 'xCaracSer ', 01, 30, 0, CTe.Compl.xCaracSer, '');
  Gerador.wCampo(tcStr, '#062', 'xEmi      ', 01, 20, 0, CTe.Compl.xEmi, '');

  (**)GerarFluxo;

  if (TpDataPeriodoToStr(CTe.Compl.Entrega.TipoData)>='0') and
     (TpHorarioIntervaloToStr(CTe.Compl.Entrega.TipoHora)>='0')
   then (**)GerarEntrega;

  Gerador.wCampo(tcStr, '#088', 'origCalc ', 01, 40, 0, CTe.Compl.origCalc, DSC_ORIGCALC);
  Gerador.wCampo(tcStr, '#089', 'destCalc ', 01, 40, 0, CTe.Compl.destCalc, DSC_DESTCALC);
  Gerador.wCampo(tcStr, '#090', 'xObs     ', 01, 2000, 0, CTe.Compl.xObs, DSC_XOBS);

  (**)GerarObsCont;
  (**)GerarObsFisco;

  Gerador.wGrupo('/compl');
end;

procedure TCTeW.GerarFluxo;
var
  i: integer;
begin
 if (CTe.Compl.fluxo.xOrig<>'') or (CTe.Compl.fluxo.pass.Count>0) or
    (CTe.Compl.fluxo.xDest<>'') or (CTe.Compl.fluxo.xRota<>'')
  then begin
   Gerador.wGrupo('fluxo', '#063'); //fluxo
   Gerador.wCampo(tcStr, '#064', 'xOrig ', 01, 15, 0, CTe.Compl.fluxo.xOrig, '');

   for i := 0 to CTe.Compl.fluxo.pass.Count - 1 do
   begin
    Gerador.wGrupo('pass', '#065');
    Gerador.wCampo(tcStr, '#066', 'xPass ', 01, 15, 1, CTe.Compl.fluxo.pass[i].xPass, '');
    Gerador.wGrupo('/pass');
   end;
   if CTe.Compl.fluxo.pass.Count > 990 then
    Gerador.wAlerta('#065', 'pass', '', ERR_MSG_MAIOR_MAXIMO + '990');

   Gerador.wCampo(tcStr, '#067', 'xDest ', 01, 15, 0, CTe.Compl.fluxo.xDest, '');
   Gerador.wCampo(tcStr, '#068', 'xRota ', 01, 10, 0, CTe.Compl.fluxo.xRota, '');
   Gerador.wGrupo('/fluxo');
  end;
end;

procedure TCTeW.GerarEntrega;
begin
  Gerador.wGrupo('Entrega', '#069'); //Entrega

  case CTe.Compl.Entrega.TipoData of
   tdSemData: begin
       Gerador.wGrupo('semData', '#070');
       Gerador.wCampo(tcStr, '#071', 'tpPer ', 01, 01, 1, TpDataPeriodoToStr(CTe.Compl.Entrega.semData.tpPer), '');
       Gerador.wGrupo('/semData');
      end;
  tdNaData,tdAteData,tdApartirData: begin
          Gerador.wGrupo('comData', '#072');
          Gerador.wCampo(tcStr, '#073', 'tpPer ', 01, 01, 1, TpDataPeriodoToStr(CTe.Compl.Entrega.comData.tpPer), '');
          Gerador.wCampo(tcDat, '#074', 'dProg ', 10, 10, 1, CTe.Compl.Entrega.comData.dProg, '');
          Gerador.wGrupo('/comData');
         end;
   tdNoPeriodo: begin
       Gerador.wGrupo('noPeriodo', '#075');
       Gerador.wCampo(tcStr, '#076', 'tpPer ', 01, 01, 1, TpDataPeriodoToStr(CTe.Compl.Entrega.noPeriodo.tpPer), '');
       Gerador.wCampo(tcDat, '#077', 'dIni  ', 10, 10, 1, CTe.Compl.Entrega.noPeriodo.dIni, '');
       Gerador.wCampo(tcDat, '#078', 'dFim  ', 10, 10, 1, CTe.Compl.Entrega.noPeriodo.dFim, '');
       Gerador.wGrupo('/noPeriodo');
      end;
  end;

  case CTe.Compl.Entrega.TipoHora of
   thSemHorario: begin
       Gerador.wGrupo('semHora', '#079');
       Gerador.wCampo(tcStr, '#080', 'tpHor ', 01, 01, 1, TpHorarioIntervaloToStr(CTe.Compl.Entrega.semHora.tpHor), '');
       Gerador.wGrupo('/semHora');
      end;
  thNoHorario,thAteHorario,thApartirHorario: begin
          Gerador.wGrupo('comHora', '#081');
          Gerador.wCampo(tcStr, '#082', 'tpHor ', 01, 01, 1, TpHorarioIntervaloToStr(CTe.Compl.Entrega.comHora.tpHor), '');
          Gerador.wCampo(tcStr, '#083', 'hProg ', 08, 08, 1, TimeToStr(CTe.Compl.Entrega.comHora.hProg), '');
          Gerador.wGrupo('/comHora');
         end;
   thNoIntervalo: begin
       Gerador.wGrupo('noInter', '#084');
       Gerador.wCampo(tcStr, '#085', 'tpHor ', 01, 01, 1, TpHorarioIntervaloToStr(CTe.Compl.Entrega.noInter.tpHor), '');
       Gerador.wCampo(tcStr, '#086', 'hIni  ', 08, 08, 1, TimeToStr(CTe.Compl.Entrega.noInter.hIni), '');
       Gerador.wCampo(tcStr, '#087', 'hFim  ', 08, 08, 1, TimeToStr(CTe.Compl.Entrega.noInter.hFim), '');
       Gerador.wGrupo('/noInter');
      end;
  end;

  Gerador.wGrupo('/Entrega');
end;

procedure TCTeW.GerarObsCont;
var
  i: integer;
begin
  for i := 0 to CTe.Compl.ObsCont.Count - 1 do
  begin
   Gerador.wGrupo('ObsCont xCampo="' + CTe.Compl.ObsCont[i].xCampo + '"', '#092');
   Gerador.wCampo(tcStr, '#093', 'xTexto ', 01, 160, 1, CTe.Compl.ObsCont[i].xTexto, 'xTexto do ObsCont');
   Gerador.wGrupo('/ObsCont');
  end;
  if CTe.Compl.ObsCont.Count > 10 then
    Gerador.wAlerta('#091', 'ObsCont', 'Obs do Contribuinte', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TCTeW.GerarObsFisco;
var
  i: integer;
begin
  for i := 0 to CTe.Compl.ObsFisco.Count - 1 do
  begin
   Gerador.wGrupo('ObsFisco xCampo="' + CTe.Compl.ObsFisco[i].xCampo + '"', '#095');
   Gerador.wCampo(tcStr, '#096', 'xTexto ', 01, 60, 1, CTe.Compl.ObsFisco[i].xTexto, 'xTexto do ObsFisco');
   Gerador.wGrupo('/ObsFisco');
  end;
  if CTe.Compl.ObsFisco.Count > 10 then
    Gerador.wAlerta('#094', 'ObsFisco', 'Obs ao Fisco', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TCTeW.GerarEmit;
begin
  Gerador.wGrupo('emit', '#097');
  Gerador.wCampoCNPJ('#098', CTe.Emit.CNPJ, CODIGO_BRASIL, True);
  Gerador.wCampo(tcStr, '#099', 'IE    ', 02, 14, 1, SomenteNumeros(CTe.Emit.IE), DSC_IE);

  if (FOpcoes.ValidarInscricoes)
   then if not ValidarIE(CTe.Emit.IE, CTe.Emit.enderEmit.UF) then
         Gerador.wAlerta('#099', 'IE', DSC_IE, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#100', 'xNome ', 01, 60, 1, CTe.Emit.xNome, DSC_XNOME);
  Gerador.wCampo(tcStr, '#101', 'xFant ', 01, 60, 0, CTe.Emit.xFant, DSC_XFANT);

  (**)GerarEnderEmit;
  Gerador.wGrupo('/emit');
end;

procedure TCTeW.GerarEnderEmit;
var
  cMun: integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL,
                                      CTe.Emit.enderEmit.UF,
                                      CTe.Emit.enderEmit.xMun,
                                      CTe.Emit.EnderEmit.cMun);
  Gerador.wGrupo('enderEmit', '#102');
  Gerador.wCampo(tcStr, '#103', 'xLgr   ', 01, 60, 1, CTe.Emit.enderEmit.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#104', 'nro    ', 01, 60, 1, ExecutarAjusteTagNro(FOpcoes.FAjustarTagNro, CTe.Emit.enderEmit.nro), DSC_NRO);
  Gerador.wCampo(tcStr, '#105', 'xCpl   ', 01, 60, 0, CTe.Emit.enderEmit.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#106', 'xBairro', 01, 60, 1, CTe.Emit.enderEmit.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#107', 'cMun   ', 07, 07, 1, cMun, DSC_CMUN);
  if not ValidarMunicipio(CTe.Emit.EnderEmit.cMun) then
    Gerador.wAlerta('#107', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#108', 'xMun   ', 01, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#109', 'CEP    ', 08, 08, 0, CTe.Emit.enderEmit.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#110', 'UF     ', 02, 02, 1, xUF, DSC_UF);
  if not ValidarUF(xUF) then
    Gerador.wAlerta('#110', 'UF', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcInt, '#111', 'cPais  ', 04, 04, 0, CODIGO_BRASIL, DSC_CPAIS); // Conforme NT-2009/01
  Gerador.wCampo(tcStr, '#112', 'xPais  ', 01, 60, 0, CTe.Emit.enderEmit.xPais, DSC_XPAIS);
  Gerador.wCampo(tcStr, '#111', 'fone   ', 07, 12, 0, somenteNumeros(CTe.Emit.EnderEmit.fone), DSC_FONE);
  Gerador.wGrupo('/enderEmit');
end;

procedure TCTeW.GerarRem;
begin
  if (CTe.Rem.CNPJCPF <> '') or
     (CTe.Rem.xNome <> '') then
    begin
      Gerador.wGrupo('rem', '#112');
      Gerador.wCampoCNPJCPF('#113', '#114', CTe.Rem.CNPJCPF, CODIGO_BRASIL);

      if Trim(CTe.Rem.IE) = 'ISENTO'
       then Gerador.wCampo(tcStr, '#115', 'IE ', 00, 14, 1, CTe.Rem.IE, DSC_IE)
       else Gerador.wCampo(tcStr, '#115', 'IE ', 00, 14, 1, SomenteNumeros(CTe.Rem.IE), DSC_IE);

      if (FOpcoes.ValidarInscricoes)
       then if not ValidarIE(CTe.Rem.IE, CTe.Rem.EnderReme.UF) then
        Gerador.wAlerta('#115', 'IE', DSC_IE, ERR_MSG_INVALIDO);

      if CTe.Ide.tpAmb = taHomologacao
       then Gerador.wCampo(tcStr, '#116', 'xNome  ', 01, 60, 1, xRazao, DSC_XNOME)
       else Gerador.wCampo(tcStr, '#116', 'xNome  ', 01, 60, 1, CTe.Rem.xNome, DSC_XNOME);
      Gerador.wCampo(tcStr, '#117', 'xFant  ', 01, 60, 0, CTe.Rem.xFant, DSC_XFANT);
      Gerador.wCampo(tcStr, '#118', 'fone   ', 07, 12, 0, somenteNumeros(CTe.Rem.fone), DSC_FONE);

      (**)GerarEnderReme;
      (**)GerarInfNF;
      (**)GerarInfNFE;
      (**)GerarInfOutros;
      Gerador.wGrupo('/rem');
    end;
end;

procedure TCTeW.GerarEnderReme;
var
  cMun: integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CTe.Rem.EnderReme.cPais,
                                      CTe.Rem.EnderReme.UF,
                                      CTe.Rem.EnderReme.xMun,
                                      CTe.Rem.EnderReme.cMun);
  Gerador.wGrupo('enderReme', '#119');
  Gerador.wCampo(tcStr, '#120', 'xLgr    ', 01, 255, 1, CTe.Rem.EnderReme.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#121', 'nro     ', 01, 60, 1, ExecutarAjusteTagNro(FOpcoes.FAjustarTagNro, CTe.Rem.EnderReme.nro), DSC_NRO);
  Gerador.wCampo(tcStr, '#122', 'xCpl    ', 01, 60, 0, CTe.Rem.EnderReme.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#123', 'xBairro ', 01, 60, 1, CTe.Rem.EnderReme.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#124', 'cMun    ', 07, 07, 1, cMun, DSC_CMUN);
  if not ValidarMunicipio(CTe.Rem.EnderReme.cMun) then
    Gerador.wAlerta('#124', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#125', 'xMun    ', 01, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#126', 'CEP     ', 08, 08, 0, CTe.Rem.EnderReme.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#127', 'UF      ', 02, 02, 1, xUF, DSC_UF);
  if not ValidarUF(xUF) then
    Gerador.wAlerta('#127', 'UF', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcInt, '#128', 'cPais   ', 04, 04, 0, CTe.Rem.EnderReme.cPais, DSC_CPAIS); // Conforme NT-2009/01
  Gerador.wCampo(tcStr, '#129', 'xPais   ', 01, 60, 0, CTe.Rem.EnderReme.xPais, DSC_XPAIS);
  Gerador.wGrupo('/enderReme');
end;

procedure TCTeW.GerarInfNF;
var
  i: integer;
begin
  for i := 0 to CTe.Rem.InfNF.Count - 1 do
  begin
    Gerador.wGrupo('infNF', '#131');
    Gerador.wCampo(tcStr, '#132', 'nRoma ', 01, 20, 0, CTe.Rem.InfNF[i].nRoma, '');
    Gerador.wCampo(tcStr, '#133', 'nPed  ', 01, 20, 0, CTe.Rem.InfNF[i].nPed, '');
    Gerador.wCampo(tcStr, '#135', 'serie ', 01, 03, 1, CTe.Rem.InfNF[i].serie, DSC_SERIE);
    Gerador.wCampo(tcEsp, '#136', 'nDoc  ', 01, 20, 1, SomenteNumeros(CTe.Rem.InfNF[i].nDoc), DSC_NDOC);
    Gerador.wCampo(tcDat, '#137', 'dEmi  ', 10, 10, 1, CTe.Rem.InfNF[i].dEmi, DSC_DEMI);
    Gerador.wCampo(tcDe2, '#138', 'vBC   ', 01, 15, 1, CTe.Rem.InfNF[i].vBC, DSC_VBCICMS);
    Gerador.wCampo(tcDe2, '#139', 'vICMS ', 01, 15, 1, CTe.Rem.InfNF[i].vICMS, DSC_VICMS);
    Gerador.wCampo(tcDe2, '#140', 'vBCST ', 01, 15, 1, CTe.Rem.InfNF[i].vBCST, DSC_VBCST);
    Gerador.wCampo(tcDe2, '#141', 'vST   ', 01, 15, 1, CTe.Rem.InfNF[i].vST, DSC_VST);
    Gerador.wCampo(tcDe2, '#142', 'vProd ', 01, 15, 1, CTe.Rem.InfNF[i].vProd, DSC_VPROD);
    Gerador.wCampo(tcDe2, '#143', 'vNF   ', 01, 15, 1, CTe.Rem.InfNF[i].vNF, DSC_VNF);
    Gerador.wCampo(tcInt, '#144', 'nCFOP ', 04, 04, 1, CTe.Rem.InfNF[i].nCFOP, DSC_CFOP);
    Gerador.wCampo(tcDe3, '#145', 'nPeso ', 01, 15, 0, CTe.Rem.InfNF[i].nPeso, DSC_PESO);
    Gerador.wCampo(tcStr, '#146', 'PIN   ', 02, 09, 0, CTe.Rem.InfNF[i].PIN, DSC_ISUF);
    if (FOpcoes.ValidarInscricoes) and (CTe.Rem.InfNF[i].PIN <> '') then
      if not ValidarISUF(CTe.Rem.InfNF[i].PIN) then
        Gerador.wAlerta('#146', 'PIN', DSC_ISUF, ERR_MSG_INVALIDO);

    if (CTe.Rem.InfNF[i].locRet.CNPJCPF <> '') or
       (CTe.Rem.InfNF[i].locRet.xNome <> '') then
    begin
      Gerador.wGrupo('locRet', '#147');
      Gerador.wCampoCNPJCPF('#148', '#149', CTe.Rem.InfNF[i].locRet.CNPJCPF, CODIGO_BRASIL);
      Gerador.wCampo(tcStr, '#150', 'xNome   ', 01, 60, 1, CTe.Rem.InfNF[i].locRet.xNome, DSC_XNOME);
      Gerador.wCampo(tcStr, '#151', 'xLgr    ', 01, 255, 1, CTe.Rem.InfNF[i].locRet.xLgr, DSC_XLGR);
      Gerador.wCampo(tcStr, '#152', 'nro     ', 01, 60, 1, ExecutarAjusteTagNro(FOpcoes.FAjustarTagNro, CTe.Rem.InfNF[i].locRet.nro), DSC_NRO);
      Gerador.wCampo(tcStr, '#153', 'xCpl    ', 01, 60, 0, CTe.Rem.InfNF[i].locRet.xCpl, DSC_XCPL);
      Gerador.wCampo(tcStr, '#154', 'xBairro ', 01, 60, 1, CTe.Rem.InfNF[i].locRet.xBairro, DSC_XBAIRRO);
      Gerador.wCampo(tcInt, '#155', 'cMun    ', 07, 07, 1, CTe.Rem.InfNF[i].locRet.cMun, DSC_CMUN);
      if not ValidarMunicipio(CTe.Rem.InfNF[i].locRet.cMun) then
        Gerador.wAlerta('#155', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);
      Gerador.wCampo(tcStr, '#156', 'xMun     ', 01, 60, 1, CTe.Rem.InfNF[i].locRet.xMun, DSC_XMUN);
      Gerador.wCampo(tcStr, '#157', 'UF       ', 02, 02, 1, CTe.Rem.InfNF[i].locRet.UF, DSC_UF);
      if not ValidarUF(CTe.Rem.InfNF[i].locRet.UF) then
        Gerador.wAlerta('#157', 'UF', DSC_UF, ERR_MSG_INVALIDO);
      Gerador.wGrupo('/locRet');
    end;

    Gerador.wGrupo('/infNF');
  end;
  if CTe.Rem.InfNF.Count > 990 then
    Gerador.wAlerta('#131', 'infNF', DSC_INFNF, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarInfNFE;
var
  i: integer;
begin
  for i := 0 to CTe.Rem.InfNFE.Count - 1 do
  begin
    Gerador.wGrupo('infNFe', '#158');
    Gerador.wCampo(tcEsp, '#159', 'chave', 44, 44, 1, SomenteNumeros(CTe.Rem.InfNFE[i].chave), DSC_REFNFE);
    // Incluido por Italo em 13/05/2012
    if SomenteNumeros(CTe.Rem.InfNFE[i].chave) <> '' then
     if not ValidarChave('NFe' + SomenteNumeros(CTe.Rem.InfNFE[i].chave)) then
      Gerador.wAlerta('#159', 'chave', DSC_REFNFE, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#160', 'PIN   ', 02, 09, 0, CTe.Rem.InfNFE[i].PIN, DSC_ISUF);
    if (FOpcoes.ValidarInscricoes) and (CTe.Rem.InfNFE[i].PIN <> '') then
      if not ValidarISUF(CTe.Rem.InfNFE[i].PIN) then
        Gerador.wAlerta('#160', 'PIN', DSC_ISUF, ERR_MSG_INVALIDO);

    Gerador.wGrupo('/infNFe');
  end;
  if CTe.Rem.InfNFE.Count > 990 then
    Gerador.wAlerta('#158', 'infNFe', DSC_INFNFE, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarInfOutros;
var
  i: integer;
begin
  for i := 0 to CTe.Rem.InfOutros.Count - 1 do
  begin
    Gerador.wGrupo('infOutros', '#161');
    Gerador.wCampo(tcStr, '#162', 'tpDoc       ', 02, 02, 1, TpDocumentoToStr(CTe.Rem.InfOutros[i].tpDoc), DSC_TPDOC);
    Gerador.wCampo(tcStr, '#163', 'descOutros  ', 01, 100, 0, CTe.Rem.InfOutros[i].descOutros, DSC_OUTROS);
    Gerador.wCampo(tcStr, '#164', 'nDoc        ', 01, 20, 0, CTe.Rem.InfOutros[i].nDoc, DSC_NRO);
    Gerador.wCampo(tcDat, '#165', 'dEmi        ', 10, 10, 1, CTe.Rem.InfOutros[i].dEmi, DSC_DEMI);
    Gerador.wCampo(tcDe2, '#166', 'vDocFisc    ', 01, 15, 0, CTe.Rem.InfOutros[i].vDocFisc, DSC_VDOC);
    Gerador.wGrupo('/infOutros');
  end;
  if CTe.Rem.InfOutros.Count > 990 then
    Gerador.wAlerta('#161', 'infOutros', DSC_INFOUTRO, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarExped;
begin
  if (CTe.Exped.CNPJCPF <> '') or
     (CTe.Exped.xNome <> '') then
  begin
    Gerador.wGrupo('exped', '#167');
    Gerador.wCampoCNPJCPF('#168', '#169', CTe.Exped.CNPJCPF, CODIGO_BRASIL);

    if Trim(CTe.Exped.IE) = 'ISENTO'
     then Gerador.wCampo(tcStr, '#170', 'IE ', 00, 14, 1, CTe.Exped.IE, DSC_IE)
     else Gerador.wCampo(tcStr, '#170', 'IE ', 00, 14, 0, SomenteNumeros(CTe.Exped.IE), DSC_IE);

    if (FOpcoes.ValidarInscricoes)
     then if not ValidarIE(CTe.Exped.IE, CTe.Exped.EnderExped.UF) then
      Gerador.wAlerta('#170', 'IE', DSC_IE, ERR_MSG_INVALIDO);

    if CTe.Ide.tpAmb = taHomologacao
     then Gerador.wCampo(tcStr, '#171', 'xNome  ', 01, 60, 1, xRazao, DSC_XNOME)
     else Gerador.wCampo(tcStr, '#171', 'xNome  ', 01, 60, 1, CTe.Exped.xNome, DSC_XNOME);
    Gerador.wCampo(tcStr, '#172', 'fone   ', 07, 12, 0, somenteNumeros(CTe.Exped.fone), DSC_FONE);

    (**)GerarEnderExped;
    Gerador.wGrupo('/exped');
  end;
end;

procedure TCTeW.GerarEnderExped;
var
  cMun: integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CTe.Exped.EnderExped.cPais,
                                      CTe.Exped.EnderExped.UF,
                                      CTe.Exped.EnderExped.xMun,
                                      CTe.Exped.EnderExped.cMun);
  Gerador.wGrupo('enderExped', '#173');
  Gerador.wCampo(tcStr, '#174', 'xLgr    ', 01, 255, 1, CTe.Exped.EnderExped.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#175', 'nro     ', 01, 60, 1, ExecutarAjusteTagNro(FOpcoes.FAjustarTagNro, CTe.Exped.EnderExped.nro), DSC_NRO);
  Gerador.wCampo(tcStr, '#176', 'xCpl    ', 01, 60, 0, CTe.Exped.EnderExped.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#177', 'xBairro ', 01, 60, 1, CTe.Exped.EnderExped.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#178', 'cMun    ', 07, 07, 1, cMun, DSC_CMUN);
  if not ValidarMunicipio(CTe.Exped.EnderExped.cMun) then
    Gerador.wAlerta('#178', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#179', 'xMun    ', 01, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#180', 'CEP     ', 08, 08, 0, CTe.Exped.EnderExped.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#181', 'UF      ', 02, 02, 1, xUF, DSC_UF);
  if not ValidarUF(xUF) then
    Gerador.wAlerta('#181', 'UF', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcInt, '#182', 'cPais   ', 04, 04, 0, CTe.Exped.EnderExped.cPais, DSC_CPAIS); // Conforme NT-2009/01
  Gerador.wCampo(tcStr, '#183', 'xPais   ', 01, 60, 0, CTe.Exped.EnderExped.xPais, DSC_XPAIS);
  Gerador.wGrupo('/enderExped');
end;

procedure TCTeW.GerarReceb;
begin
  if (CTe.Receb.CNPJCPF <> '') or
     (CTe.Receb.xNome <> '') then
  Begin
    Gerador.wGrupo('receb', '#185');
    Gerador.wCampoCNPJCPF('#186', '#187', CTe.Receb.CNPJCPF, CODIGO_BRASIL);

    if Trim(CTe.Receb.IE) = 'ISENTO'
     then Gerador.wCampo(tcStr, '#188', 'IE ', 00, 14, 1, CTe.Receb.IE, DSC_IE)
     else Gerador.wCampo(tcStr, '#188', 'IE ', 00, 14, 0, SomenteNumeros(CTe.Receb.IE), DSC_IE);

    if (FOpcoes.ValidarInscricoes)
     then if not ValidarIE(CTe.Receb.IE, CTe.Receb.EnderReceb.UF) then
      Gerador.wAlerta('#188', 'IE', DSC_IE, ERR_MSG_INVALIDO);

    if CTe.Ide.tpAmb = taHomologacao
     then Gerador.wCampo(tcStr, '#189', 'xNome  ', 01, 60, 1, xRazao, DSC_XNOME)
     else Gerador.wCampo(tcStr, '#189', 'xNome  ', 01, 60, 1, CTe.Receb.xNome, DSC_XNOME);
    Gerador.wCampo(tcStr, '#190', 'fone   ', 07, 12, 0, somenteNumeros(CTe.Receb.fone), DSC_FONE);

    (**)GerarEnderReceb;
    Gerador.wGrupo('/receb');
  end;
end;

procedure TCTeW.GerarEnderReceb;
var
  cMun: integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CTe.Receb.EnderReceb.cPais,
                                      CTe.Receb.EnderReceb.UF,
                                      CTe.Receb.EnderReceb.xMun,
                                      CTe.Receb.EnderReceb.cMun);
  Gerador.wGrupo('enderReceb', '#191');
  Gerador.wCampo(tcStr, '#192', 'xLgr    ', 01, 255, 1, CTe.Receb.EnderReceb.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#193', 'nro     ', 01, 60, 1, ExecutarAjusteTagNro(FOpcoes.FAjustarTagNro, CTe.Receb.EnderReceb.nro), DSC_NRO);
  Gerador.wCampo(tcStr, '#194', 'xCpl    ', 01, 60, 0, CTe.Receb.EnderReceb.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#195', 'xBairro ', 01, 60, 1, CTe.Receb.EnderReceb.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#196', 'cMun    ', 07, 07, 1, cMun, DSC_CMUN);
  if not ValidarMunicipio(CTe.Receb.EnderReceb.cMun) then
    Gerador.wAlerta('#196', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#197', 'xMun    ', 01, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#198', 'CEP     ', 08, 08, 0, CTe.Receb.EnderReceb.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#199', 'UF      ', 02, 02, 1, xUF, DSC_UF);
  if not ValidarUF(xUF) then
    Gerador.wAlerta('#199', 'UF', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcInt, '#200', 'cPais   ', 04, 04, 0, CTe.Receb.EnderReceb.cPais, DSC_CPAIS); // Conforme NT-2009/01
  Gerador.wCampo(tcStr, '#201', 'xPais   ', 01, 60, 0, CTe.Receb.EnderReceb.xPais, DSC_XPAIS);
  Gerador.wGrupo('/enderReceb');
end;

procedure TCTeW.GerarDest;
begin
  if (CTe.Dest.CNPJCPF <> '') or
     (CTe.Dest.xNome <> '') then
    begin
      Gerador.wGrupo('dest', '#203');
      Gerador.wCampoCNPJCPF('#204', '#205', CTe.Dest.CNPJCPF, CODIGO_BRASIL);

      if CTe.Dest.IE <> ''
       then begin
        if Trim(CTe.Dest.IE) = 'ISENTO'
         then Gerador.wCampo(tcStr, '#206', 'IE ', 00, 14, 1, CTe.Dest.IE, DSC_IE)
         else Gerador.wCampo(tcStr, '#206', 'IE ', 00, 14, 1, SomenteNumeros(CTe.Dest.IE), DSC_IE);

        if (FOpcoes.ValidarInscricoes)
         then if not ValidarIE(CTe.Dest.IE, CTe.Dest.EnderDest.UF) then
          Gerador.wAlerta('#206', 'IE', DSC_IE, ERR_MSG_INVALIDO);
       end;

      if CTe.Ide.tpAmb = taHomologacao
       then Gerador.wCampo(tcStr, '#207', 'xNome  ', 01, 60, 1, xRazao, DSC_XNOME)
       else Gerador.wCampo(tcStr, '#207', 'xNome  ', 01, 60, 1, CTe.Dest.xNome, DSC_XNOME);
      Gerador.wCampo(tcStr, '#208', 'fone   ', 07, 12, 0, somenteNumeros(CTe.Dest.fone), DSC_FONE);
      Gerador.wCampo(tcStr, '#209', 'ISUF   ', 08, 09, 0, CTe.Dest.ISUF, DSC_ISUF);
      if (FOpcoes.ValidarInscricoes) and (CTe.Dest.ISUF <> '') then
        if not ValidarISUF(CTe.Dest.ISUF) then
          Gerador.wAlerta('#209', 'ISUF', DSC_ISUF, ERR_MSG_INVALIDO);

      (**)GerarEnderDest;
      (**)GerarLocEnt;
      Gerador.wGrupo('/dest');
    end;
end;

procedure TCTeW.GerarEnderDest;
var
  cMun: integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CTe.Dest.EnderDest.cPais,
                                      CTe.Dest.EnderDest.UF,
                                      CTe.Dest.EnderDest.xMun,
                                      CTe.Dest.EnderDest.cMun);
  Gerador.wGrupo('enderDest', '#210');
  Gerador.wCampo(tcStr, '#211', 'xLgr   ', 01, 255, 1, CTe.Dest.EnderDest.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#212', 'nro    ', 01, 60, 1, ExecutarAjusteTagNro(FOpcoes.FAjustarTagNro, CTe.Dest.EnderDest.nro), DSC_NRO);
  Gerador.wCampo(tcStr, '#213', 'xCpl   ', 01, 60, 0, CTe.Dest.EnderDest.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#214', 'xBairro', 01, 60, 1, CTe.Dest.EnderDest.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#215', 'cMun   ', 07, 07, 1, cMun, DSC_CMUN);
  if not ValidarMunicipio(CTe.Dest.EnderDest.cMun) then
    Gerador.wAlerta('#215', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#216', 'xMun   ', 01, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#217', 'CEP    ', 08, 08, 0, CTe.Dest.EnderDest.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#218', 'UF     ', 02, 02, 1, xUF, DSC_UF);
  if not ValidarUF(xUF) then
    Gerador.wAlerta('#218', 'UF', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcInt, '#219', 'cPais  ', 04, 04, 0, CTe.Dest.EnderDest.cPais, DSC_CPAIS); // Conforme NT-2009/01
  Gerador.wCampo(tcStr, '#220', 'xPais  ', 01, 60, 0, CTe.Dest.EnderDest.xPais, DSC_XPAIS);
  Gerador.wGrupo('/enderDest');
end;

procedure TCTeW.GerarLocEnt;
begin
  if (CTe.Dest.locEnt.CNPJCPF <> '') or
     (CTe.Dest.locEnt.xNome <> '') then
  begin
    Gerador.wGrupo('locEnt', '#222');
    Gerador.wCampoCNPJCPF('#223', '#224', CTe.Dest.locEnt.CNPJCPF, CODIGO_BRASIL);
    Gerador.wCampo(tcStr, '#225', 'xNome  ', 01, 60, 1, CTe.Dest.locEnt.xNome, DSC_XNOME);
    Gerador.wCampo(tcStr, '#226', 'xLgr   ', 01, 255, 1, CTe.Dest.locEnt.xLgr, DSC_XLGR);
    Gerador.wCampo(tcStr, '#227', 'nro    ', 01, 60, 1, ExecutarAjusteTagNro(FOpcoes.FAjustarTagNro, CTe.Dest.locEnt.nro), DSC_NRO);
    Gerador.wCampo(tcStr, '#228', 'xCpl   ', 01, 60, 0, CTe.Dest.locEnt.xCpl, DSC_XCPL);
    Gerador.wCampo(tcStr, '#229', 'xBairro', 01, 60, 1, CTe.Dest.locEnt.xBairro, DSC_XBAIRRO);
    Gerador.wCampo(tcInt, '#230', 'cMun   ', 07, 07, 1, CTe.Dest.locEnt.cMun, DSC_CMUN);
    if not ValidarMunicipio(CTe.Dest.locEnt.cMun) then
      Gerador.wAlerta('#230', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);
    Gerador.wCampo(tcStr, '#231', 'xMun   ', 01, 60, 1, CTe.Dest.locEnt.xMun, DSC_XMUN);
    Gerador.wCampo(tcStr, '#232', 'UF     ', 02, 02, 1, CTe.Dest.locEnt.UF, DSC_UF);
    if not ValidarUF(CTe.Dest.locEnt.UF) then
      Gerador.wAlerta('#232', 'UF', DSC_UF, ERR_MSG_INVALIDO);
    Gerador.wGrupo('/locEnt');
  end;
end;

procedure TCTeW.GerarVPrest;
begin
  Gerador.wGrupo('vPrest', '#233');
  Gerador.wCampo(tcDe2, '#234', 'vTPrest ', 01, 15, 1, CTe.vPrest.vTPrest, DSC_VTPREST);
  Gerador.wCampo(tcDe2, '#235', 'vRec    ', 01, 15, 1, CTe.vPrest.vRec, DSC_VREC);

  (**)GerarComp;
  Gerador.wGrupo('/vPrest');
end;

procedure TCTeW.GerarComp;
var
  i: integer;
begin
  for i := 0 to CTe.vPrest.comp.Count - 1 do
  begin
    if (CTe.vPrest.comp[i].xNome <> '') and
      (CTe.vPrest.comp[i].vComp <> 0) then
      begin
        Gerador.wGrupo('Comp', '#236');
        Gerador.wCampo(tcStr, '#237', 'xNome ', 01, 15, 1, CTe.vPrest.comp[i].xNome, DSC_XNOMEC);
        Gerador.wCampo(tcDe2, '#238', 'vComp ', 01, 15, 1, CTe.vPrest.comp[i].vComp, DSC_VCOMP);
        Gerador.wGrupo('/Comp');
      end;
  end;
end;

procedure TCTeW.GerarImp;
begin
  Gerador.wGrupo('imp', '#239');
  (**)GerarICMS;
  Gerador.wCampo(tcDe2, '#275', 'vTotTrib   ', 01, 15, 0, CTe.Imp.vTotTrib, DSC_VCOMP);
  Gerador.wCampo(tcStr, '#275', 'infAdFisco ', 01, 2000, 0, CTe.Imp.InfAdFisco, DSC_INFADFISCO);
  Gerador.wGrupo('/imp');
end;

procedure TCTeW.GerarICMS;
begin
  Gerador.wGrupo('ICMS', '#240');

  if CTe.Imp.ICMS.SituTrib = cst00 then
    (**)GerarCST00
  else if CTe.Imp.ICMS.SituTrib = cst20 then
    (**)GerarCST20
  else if ((CTe.Imp.ICMS.SituTrib = cst40) or
           (CTe.Imp.ICMS.SituTrib = cst41) or
           (CTe.Imp.ICMS.SituTrib = cst51)) then
    (**)GerarCST45
  else if CTe.Imp.ICMS.SituTrib = cst80 then
    (**)GerarCST80
  else if CTe.Imp.ICMS.SituTrib = cst81 then
    (**)GerarCST81
  else if CTe.Imp.ICMS.SituTrib = cst90 then
    (**)GerarCST90;

  Gerador.wGrupo('/ICMS');
end;

procedure TCTeW.GerarCST00;
begin
  Gerador.wGrupo('CST00', '#241');
  Gerador.wCampo(tcStr, '#242', 'CST   ', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.CST00.CST), DSC_CST);
  Gerador.wCampo(tcDe2, '#243', 'vBC   ', 01, 15, 1, CTe.Imp.ICMS.CST00.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#244', 'pICMS ', 01, 05, 1, CTe.Imp.ICMS.CST00.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#245', 'vICMS ', 01, 15, 1, CTe.Imp.ICMS.CST00.vICMS, DSC_VICMS);
  Gerador.wGrupo('/CST00');
end;

procedure TCTeW.GerarCST20;
begin
  Gerador.wGrupo('CST20', '#246');
  Gerador.wCampo(tcStr, '#247', 'CST    ', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.CST20.CST), DSC_CST);
  Gerador.wCampo(tcDe2, '#248', 'pRedBC ', 01, 05, 1, CTe.Imp.ICMS.CST20.pRedBC, DSC_PREDBC);
  Gerador.wCampo(tcDe2, '#249', 'vBC    ', 01, 15, 1, CTe.Imp.ICMS.CST20.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#250', 'pICMS  ', 01, 05, 1, CTe.Imp.ICMS.CST20.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#251', 'vICMS  ', 01, 15, 1, CTe.Imp.ICMS.CST20.vICMS, DSC_VICMS);
  Gerador.wGrupo('/CST20');
end;

procedure TCTeW.GerarCST45;
begin
  Gerador.wGrupo('CST45', '#252');
  Gerador.wCampo(tcStr, '#253', 'CST ', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.CST45.CST), DSC_CST);
  Gerador.wGrupo('/CST45');
end;

procedure TCTeW.GerarCST80;
begin
  Gerador.wGrupo('CST80', 'J14');
  Gerador.wCampo(tcStr, 'J30', 'CST   ', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.CST80.CST), DSC_CST);
  Gerador.wCampo(tcDe2, 'J41', 'vBC   ', 01, 15, 1, CTe.Imp.ICMS.CST80.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, 'J42', 'pICMS ', 01, 05, 1, CTe.Imp.ICMS.CST80.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, 'J43', 'vICMS ', 01, 15, 1, CTe.Imp.ICMS.CST80.vICMS, DSC_VICMS);
  if CTe.Imp.ICMS.CST80.vCred > 0 then
   Gerador.wCampo(tcDe2, 'J44', 'vCred ', 01, 15, 1, CTe.Imp.ICMS.CST80.vCred, DSC_VCRED);
  Gerador.wGrupo('/CST80');
end;

procedure TCTeW.GerarCST81;
begin
  Gerador.wGrupo('CST81', 'J15');
  Gerador.wCampo(tcStr, 'J30', 'CST     ', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.CST81.CST), DSC_CST);
  if CTe.Imp.ICMS.CST81.pRedBC > 0 then
   Gerador.wCampo(tcDe2, 'J51', 'pRedBC ', 01, 05, 1, CTe.Imp.ICMS.CST81.pRedBC, DSC_PREDBC);
  Gerador.wCampo(tcDe2, 'J52', 'vBC     ', 01, 15, 1, CTe.Imp.ICMS.CST81.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, 'J53', 'pICMS   ', 01, 05, 1, CTe.Imp.ICMS.CST81.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, 'J54', 'vICMS   ', 01, 15, 1, CTe.Imp.ICMS.CST81.vICMS, DSC_VICMS);
  Gerador.wGrupo('/CST81');
end;

procedure TCTeW.GerarCST90;
begin
  Gerador.wGrupo('CST90', '#260');
  Gerador.wCampo(tcStr, '#261', 'CST      ', 02, 02, 1, CSTICMSTOStr(CTe.Imp.ICMS.CST90.CST), DSC_CST);
  if CTe.Imp.ICMS.CST90.pRedBC > 0 then
    Gerador.wCampo(tcDe2, '#262', 'pRedBC ', 01, 05, 1, CTe.Imp.ICMS.CST90.pRedBC, DSC_PREDBC);
  Gerador.wCampo(tcDe2, '#263', 'vBC      ', 01, 15, 1, CTe.Imp.ICMS.CST90.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#264', 'pICMS    ', 01, 05, 1, CTe.Imp.ICMS.CST90.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#265', 'vICMS    ', 01, 15, 1, CTe.Imp.ICMS.CST90.vICMS, DSC_VICMS);
  if CTe.Imp.ICMS.CST90.vCred > 0 then
    Gerador.wCampo(tcDe2, '#266', 'vCred  ', 01, 15, 1, CTe.Imp.ICMS.CST90.vCred, DSC_VCRED);
  Gerador.wGrupo('/CST90');
end;

procedure TCTeW.GerarInfCTeNorm;
begin
  if (CTe.Ide.tpCTe = tcNormal) or (CTe.Ide.tpCTe = tcSubstituto) then
  begin
    Gerador.wGrupo('infCTeNorm', 'K01');
    (**)GerarinfCarga;
    (**)GerarContQt;
    if CTe.infCTeNorm.emiDocAnt.Count>0
     then (**)GerarDocAnt;
    (**)GerarInfSeg;

    case StrToInt(TpModalToStr(CTe.Ide.modal)) of
     01: (**)GerarRodo;   // Informações do Modal Rodoviário
     02: (**)GerarAereo;  // Informações do Modal Aéreo
     03: (**)GerarAquav;  // Informações do Modal Aquaviário
     04: (**)GerarFerrov; // Informações do Modal Ferroviário
     05: (**)GerarDuto;   // Informações do Modal Dutoviário
    end;

    (**)GerarPeri; // Informações de produtos classificados pela ONU como Perigosos
    (**)GerarVeicNovos;
    (**)GerarInfCTeSub;

    Gerador.wGrupo('/infCTeNorm');
  end;
end;

procedure TCTeW.GerarinfCarga;
begin
  Gerador.wGrupo('infCarga', '#277');
  Gerador.wCampo(tcDe2, '#278', 'vMerc   ', 01, 15, 1, CTe.InfCarga.vMerc, DSC_VTMERC);
  Gerador.wCampo(tcStr, '#279', 'proPred ', 01, 60, 1, CTe.InfCarga.proPred, DSC_PRED);
  Gerador.wCampo(tcStr, '#280', 'xOutCat ', 01, 30, 0, CTe.InfCarga.xOutCat, DSC_OUTCAT);

  (**)GerarInfQ;

  Gerador.wGrupo('/infCarga');
end;

procedure TCTeW.GerarInfQ;
var
  i: integer;
begin
  for i := 0 to CTe.InfCarga.InfQ.Count - 1 do
  begin
    Gerador.wGrupo('infQ', '#281');
    Gerador.wCampo(tcStr, '#282', 'cUnid  ', 02, 02, 1, UnidMedToStr(CTe.InfCarga.InfQ[i].cUnid), DSC_CUNID);
    Gerador.wCampo(tcStr, '#283', 'tpMed  ', 01, 20, 1, CTe.InfCarga.InfQ[i].tpMed, DSC_TPMED);
    Gerador.wCampo(tcDe4, '#284', 'qCarga ', 01, 15, 1, CTe.InfCarga.InfQ[i].qCarga, DSC_QTD);

    Gerador.wGrupo('/infQ');
  end;

  if CTe.InfCarga.InfQ.Count > 990 then
    Gerador.wAlerta('#281', 'infQ', DSC_INFQ, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarContQt;
var
  i, i01: integer;
begin
  for i := 0 to CTe.infCTeNorm.contQt.Count - 1 do
  begin
    Gerador.wGrupo('contQt', '#285');
    // Alterado de Integer para String por Italo em 31/01/2012
    Gerador.wCampo(tcStr, '#286', 'nCont ', 01, 20, 1, CTe.infCTeNorm.contQt[i].nCont, '');

    for i01 := 0 to CTe.infCTeNorm.contQt[i].lacContQt.Count - 1 do
    begin
      Gerador.wGrupo('lacContQt', '#287');
      Gerador.wCampo(tcStr, '#288', 'nLacre ', 01, 20, 1, CTe.infCTeNorm.contQt[i].lacContQt[i01].nLacre, '');
      Gerador.wGrupo('/lacContQt');
    end;
    if CTe.infCTeNorm.contQt[i].lacContQt.Count > 990 then
      Gerador.wAlerta('#287', 'lacContQt', '', ERR_MSG_MAIOR_MAXIMO + '990');

    Gerador.wCampo(tcDat, '#289', 'dPrev  ', 10, 10, 0, CTe.infCTeNorm.contQt[i].dPrev, '');
    Gerador.wGrupo('/contQt');
  end;
  if CTe.infCTeNorm.contQt.Count > 990 then
    Gerador.wAlerta('#285', 'contQt', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarDocAnt;
var
  i, i01, i02: integer;
begin
  Gerador.wGrupo('docAnt', '#290');

  for i := 0 to CTe.infCTeNorm.emiDocAnt.Count - 1 do
  begin
    Gerador.wGrupo('emiDocAnt', '#291');
    Gerador.wCampoCNPJCPF('#292', '#293', CTe.infCTeNorm.emiDocAnt[i].CNPJCPF, CODIGO_BRASIL);

    if Trim(CTe.infCTeNorm.emiDocAnt[i].IE) = 'ISENTO'
     then Gerador.wCampo(tcStr, '#294', 'IE ', 00, 14, 1, CTe.infCTeNorm.emiDocAnt[i].IE, DSC_IE)
     else Gerador.wCampo(tcStr, '#294', 'IE ', 02, 14, 1, SomenteNumeros(CTe.infCTeNorm.emiDocAnt[i].IE), DSC_IE);

    if (FOpcoes.ValidarInscricoes)
     then if not ValidarIE(CTe.infCTeNorm.emiDocAnt[i].IE, CTe.infCTeNorm.emiDocAnt[i].UF) then
      Gerador.wAlerta('#294', 'IE', DSC_IE, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#295', 'UF    ', 02, 02, 1, CTe.infCTeNorm.emiDocAnt[i].UF, '');
    if not ValidarUF(CTe.infCTeNorm.emiDocAnt[i].UF) then
      Gerador.wAlerta('#295', 'UF', DSC_UF, ERR_MSG_INVALIDO);
    Gerador.wCampo(tcStr, '#296', 'xNome ', 01, 60, 1, CTe.infCTeNorm.emiDocAnt[i].xNome, '');

    for i01 := 0 to CTe.infCTeNorm.emiDocAnt[i].idDocAnt.Count - 1 do
    begin
      Gerador.wGrupo('idDocAnt', '#297');

      for i02 := 0 to CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntPap.Count - 1 do
      begin
        Gerador.wGrupo('idDocAntPap', '#298');
        Gerador.wCampo(tcStr, '#299', 'tpDoc  ', 02, 02, 1, TpDocumentoAnteriorToStr(CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntPap[i02].tpDoc), '');
        Gerador.wCampo(tcStr, '#300', 'serie  ', 01, 03, 1, CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntPap[i02].serie, '');
        Gerador.wCampo(tcStr, '#301', 'subser ', 01, 02, 0, CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntPap[i02].subser, '');
        Gerador.wCampo(tcInt, '#302', 'nDoc   ', 01, 20, 1, CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntPap[i02].nDoc, '');
        Gerador.wCampo(tcDat, '#303', 'dEmi   ', 10, 10, 1, CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntPap[i02].dEmi, '');
        Gerador.wGrupo('/idDocAntPap');
      end;
      if CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntPap.Count > 990 then
        Gerador.wAlerta('#298', 'idDocAntPap', '', ERR_MSG_MAIOR_MAXIMO + '990');

      for i02 := 0 to CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntEle.Count - 1 do
      begin
        Gerador.wGrupo('idDocAntEle', '#304');
        // Alterado por Italo em 13/05/2012
        Gerador.wCampo(tcStr, '#305', 'chave ', 44, 44, 1, SomenteNumeros(CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntEle[i02].chave), '');
        // Incluido por Italo em 13/05/2012
        if SomenteNumeros(CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntEle[i02].chave) <> '' then
         if not ValidarChave('NFe' + SomenteNumeros(CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntEle[i02].chave)) then
          Gerador.wAlerta('#305', 'chave', DSC_REFCTE, ERR_MSG_INVALIDO);
        Gerador.wGrupo('/idDocAntEle');
      end;
      if CTe.infCTeNorm.emiDocAnt[i].idDocAnt[i01].idDocAntEle.Count > 990 then
        Gerador.wAlerta('#304', 'idDocAntEle', '', ERR_MSG_MAIOR_MAXIMO + '990');

      Gerador.wGrupo('/idDocAnt');
    end;
    if CTe.infCTeNorm.emiDocAnt[i].idDocAnt.Count > 2 then
      Gerador.wAlerta('#297', 'idDocAnt', '', ERR_MSG_MAIOR_MAXIMO + '02');

    Gerador.wGrupo('/emiDocAnt');
  end;
  if CTe.infCTeNorm.emiDocAnt.Count > 990 then
    Gerador.wAlerta('#291', 'emiDocAnt', '', ERR_MSG_MAIOR_MAXIMO + '990');

  Gerador.wGrupo('/docAnt');
end;

procedure TCTeW.GerarInfSeg;
var
  i: integer;
begin
  for i := 0 to CTe.InfSeg.Count - 1 do
  begin
    Gerador.wGrupo('seg', '#306');
    Gerador.wCampo(tcStr, '#307', 'respSeg ', 01, 01, 1, TpRspSeguroToStr(CTe.InfSeg[i].respSeg), DSC_RESPSEG);
    Gerador.wCampo(tcStr, '#308', 'xSeg    ', 01, 30, 0, CTe.InfSeg[i].xSeg, DSC_XSEG);
    Gerador.wCampo(tcStr, '#309', 'nApol   ', 01, 20, 0, CTe.InfSeg[i].nApol, DSC_NAPOL);
    Gerador.wCampo(tcStr, '#310', 'nAver   ', 01, 20, 0, CTe.InfSeg[i].nAver, DSC_NAVER);
    Gerador.wCampo(tcDe3, '#311', 'vMerc   ', 01, 15, 0, CTe.InfSeg[i].vMerc, DSC_VMERC);
    Gerador.wGrupo('/seg');
  end;
  if CTe.InfSeg.Count > 990 then
    Gerador.wAlerta('#306', 'seg', DSC_INFSEG, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarRodo;
begin
  Gerador.wGrupo('rodo', '#01');
  Gerador.wCampo(tcStr, '#02', 'RNTRC ', 14, 14, 1, SomenteNumeros(CTe.Rodo.RNTRC), DSC_RNTRC);
  Gerador.wCampo(tcDat, '#03', 'dPrev ', 10, 10, 1, CTe.Rodo.dPrev, DSC_DPREV);
  Gerador.wCampo(tcStr, '#04', 'lota  ', 01, 01, 1, TpLotacaoToStr(CTe.Rodo.Lota), DSC_LOTA);

  (**)GerarCTRB;
  (**)GerarOCC;

  if CTe.Rodo.Lota = ltSim
   then begin
    (**)GerarValePed;
    (**)GerarVeic;
   end;
  (**)GerarLacre;

  if CTe.Rodo.Lota = ltSim then
   (**)GerarMoto;

  Gerador.wGrupo('/rodo');
end;

procedure TCTeW.GerarCTRB;
begin
  if (CTe.Rodo.CTRB.serie<>0) or (CTe.Rodo.CTRB.nCTRB<>0)
   then begin
    Gerador.wGrupo('CTRB', 'L07');
    Gerador.wCampo(tcInt, 'L08', 'serie ', 01, 03, 1, CTe.Rodo.CTRB.serie, '');
    Gerador.wCampo(tcInt, 'L09', 'nCTRB ', 01, 06, 1, CTe.Rodo.CTRB.nCTRB, '');
    Gerador.wGrupo('/CTRB');
   end;
end;

procedure TCTeW.GerarOCC;
var
 i: Integer;
begin
  for i := 0 to CTe.Rodo.Occ.Count - 1 do
  begin
    Gerador.wGrupo('occ', '#06');
    Gerador.wCampo(tcStr, '#07', 'serie ', 01, 03, 0, CTe.Rodo.Occ[i].serie, '');
    Gerador.wCampo(tcInt, '#08', 'nOcc  ', 01, 06, 1, CTe.Rodo.Occ[i].nOcc, '');
    Gerador.wCampo(tcDat, '#09', 'dEmi  ', 10, 10, 1, CTe.Rodo.Occ[i].dEmi, '');

    Gerador.wGrupo('emiOcc', '#10');
    Gerador.wCampoCNPJ('#11', CTe.Rodo.Occ[i].EmiOCC.CNPJ, CODIGO_BRASIL, True);
    Gerador.wCampo(tcStr, '#12', 'cInt   ', 01, 10, 0, CTe.Rodo.Occ[i].EmiOCC.cInt, DSC_CINT);

    Gerador.wCampo(tcStr, '#13', 'IE ', 02, 14, 1, SomenteNumeros(CTe.Rodo.Occ[i].EmiOCC.IE), DSC_IE);

    if (FOpcoes.ValidarInscricoes)
     then if not ValidarIE(CTe.Rodo.Occ[i].EmiOCC.IE, CTe.Rodo.Occ[i].EmiOCC.UF) then
      Gerador.wAlerta('#13', 'IE', DSC_IE, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, '#14', 'UF   ', 02, 02, 1, CTe.Rodo.Occ[i].EmiOCC.UF, DSC_CUF);
    if not ValidarUF(CTe.Rodo.Occ[i].EmiOCC.UF) then
      Gerador.wAlerta('#14', 'UF', DSC_UF, ERR_MSG_INVALIDO);
    Gerador.wCampo(tcStr, '#15', 'fone ', 07, 12, 0, somenteNumeros(CTe.Rodo.Occ[i].EmiOCC.fone), DSC_FONE);
    Gerador.wGrupo('/emiOcc');

    Gerador.wGrupo('/occ');
  end;
  if CTe.Rodo.Occ.Count > 10 then
    Gerador.wAlerta('#06', 'occ', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TCTeW.GerarValePed;
var
 i: Integer;
begin
  Gerador.wGrupo('valePed', 'L23');

  Gerador.wCampo(tcStr, 'L24', 'nroRE     ', 05, 09, 0, CTe.Rodo.valePed.nroRE, '');
  Gerador.wCampo(tcDe2, 'L25', 'vTValePed ', 01, 15, 0, CTe.Rodo.valePed.vTValePed, '');
  Gerador.wCampo(tcStr, 'L26', 'respPg    ', 01, 01, 1, RspPagPedagioToStr(CTe.Rodo.valePed.respPg), '');

  if (CTe.Rodo.valePed.nroRE='')
   then begin
    for i := 0 to CTe.Rodo.valePed.disp.Count - 1 do
    begin
      Gerador.wGrupo('disp', 'L27');
      Gerador.wCampo(tcStr, 'L28', 'tpDisp ', 01, 01, 1, TpDispositivoToStr(CTe.Rodo.valePed.disp[i].tpDisp), '');
      Gerador.wCampo(tcStr, 'L29', 'xEmp   ', 01, 30, 1, CTe.Rodo.valePed.disp[i].xEmp, '');
      Gerador.wCampo(tcDat, 'L30', 'dVig   ', 10, 10, 1, CTe.Rodo.valePed.disp[i].dVig, '');
      Gerador.wCampo(tcStr, 'L31', 'nDisp  ', 01, 20, 0, CTe.Rodo.valePed.disp[i].nDisp, '');
      Gerador.wCampo(tcStr, 'L32', 'nCompC ', 01, 14, 0, CTe.Rodo.valePed.disp[i].nCompC, '');
      Gerador.wGrupo('/disp');
    end;
    if CTe.Rodo.valePed.disp.Count > 990 then
      Gerador.wAlerta('L27', 'disp', '', ERR_MSG_MAIOR_MAXIMO + '990');
   end;

  Gerador.wGrupo('/valePed');
end;

procedure TCTeW.GerarVeic;
var
  i: integer;
begin
  for i := 0 to CTe.Rodo.veic.Count - 1 do
  begin
    Gerador.wGrupo('veic', '#20');
    Gerador.wCampo(tcStr, '#21', 'cInt    ', 01, 10, 0, CTe.Rodo.veic[i].cInt, '');
    Gerador.wCampo(tcStr, '#22', 'RENAVAM ', 09, 11, 1, CTe.Rodo.veic[i].RENAVAM, '');
    Gerador.wCampo(tcStr, '#23', 'placa   ', 01, 07, 1, CTe.Rodo.veic[i].placa, '');
    Gerador.wCampo(tcInt, '#24', 'tara    ', 01, 06, 1, CTe.Rodo.veic[i].tara, '');
    Gerador.wCampo(tcInt, '#25', 'capKG   ', 01, 06, 1, CTe.Rodo.veic[i].capKG, '');
    Gerador.wCampo(tcInt, '#26', 'capM3   ', 01, 03, 1, CTe.Rodo.veic[i].capM3, '');
    Gerador.wCampo(tcStr, '#27', 'tpProp  ', 01, 01, 1, TpPropriedadeToStr(CTe.Rodo.veic[i].tpProp), '');
    Gerador.wCampo(tcStr, '#28', 'tpVeic  ', 01, 01, 1, TpVeiculoToStr(CTe.Rodo.veic[i].tpVeic), '');
    Gerador.wCampo(tcStr, '#29', 'tpRod   ', 02, 02, 1, TpRodadoToStr(CTe.Rodo.veic[i].tpRod), '');
    Gerador.wCampo(tcStr, '#30', 'tpCar   ', 02, 02, 1, TpCarroceriaToStr(CTe.Rodo.veic[i].tpCar), '');
    Gerador.wCampo(tcStr, '#31', 'UF      ', 02, 02, 1, CTe.Rodo.veic[i].UF, DSC_CUF);
    if not ValidarUF(CTe.Rodo.veic[i].UF) then
      Gerador.wAlerta('#31', 'UF', DSC_UF, ERR_MSG_INVALIDO);

    if (CTe.Rodo.veic[i].Prop.CNPJCPF <> '') or
       (CTe.Rodo.veic[i].Prop.RNTRC <> '') or
       (CTe.Rodo.veic[i].Prop.xNome <> '') then
    begin
      Gerador.wGrupo('prop', '#32');
      Gerador.wCampoCNPJCPF('#33', '#34', CTe.Rodo.veic[i].Prop.CNPJCPF, CODIGO_BRASIL);
      Gerador.wCampo(tcStr, '#35', 'RNTRC ', 14, 14, 1, SomenteNumeros(CTe.Rodo.veic[i].Prop.RNTRC), DSC_RNTRC);
      Gerador.wCampo(tcStr, '#36', 'xNome ', 01, 60, 1, CTe.Rodo.veic[i].Prop.xNome, DSC_XNOME);

      Gerador.wCampo(tcStr, '#37', 'IE ', 02, 14, 1, SomenteNumeros(CTe.Rodo.veic[i].Prop.IE), DSC_IE);
      if (FOpcoes.ValidarInscricoes)
       then if not ValidarIE(CTe.Rodo.veic[i].Prop.IE, CTe.Rodo.veic[i].Prop.UF) then
        Gerador.wAlerta('#37', 'IE', DSC_IE, ERR_MSG_INVALIDO);

      Gerador.wCampo(tcStr, '#38', 'UF     ', 02, 02, 1, CTe.Rodo.veic[i].Prop.UF, DSC_CUF);
      if not ValidarUF(CTe.Rodo.veic[i].Prop.UF) then
       Gerador.wAlerta('#38', 'UF', DSC_UF, ERR_MSG_INVALIDO);
      Gerador.wCampo(tcStr, '#39', 'tpProp ', 01, 01, 1, TpPropToStr(CTe.Rodo.veic[i].Prop.tpProp), DSC_TPPROP);
      Gerador.wGrupo('/prop');
    end;

    Gerador.wGrupo('/veic');
  end;
  if CTe.Rodo.veic.Count > 4 then
    Gerador.wAlerta('#20', 'veic', '', ERR_MSG_MAIOR_MAXIMO + '4');
end;

procedure TCTeW.GerarLacre;
var
  i: integer;
begin
  for i := 0 to CTe.Rodo.Lacres.Count - 1 do
  begin
    Gerador.wGrupo('lacRodo', '#40');
    Gerador.wCampo(tcStr, '#41', 'nLacre ', 01, 20, 1, CTe.Rodo.Lacres[i].nLacre, DSC_NLACRE);
    Gerador.wGrupo('/lacRodo');
  end;
  if CTe.Rodo.Lacres.Count > 990 then
    Gerador.wAlerta('#40', 'lacRodo', DSC_LACR, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarMoto;
var
  i: integer;
begin
  for i := 0 to CTe.Rodo.moto.Count - 1 do
  begin
    Gerador.wGrupo('moto', '#42');
    Gerador.wCampo(tcStr, '#43', 'xNome ', 01, 60, 1, CTe.Rodo.moto[i].xNome, '');
    Gerador.wCampo(tcStr, '#44', 'CPF   ', 11, 11, 1, CTe.Rodo.moto[i].CPF, '');
    Gerador.wGrupo('/moto');
  end;
  if CTe.Rodo.moto.Count > 990 then
    Gerador.wAlerta('#42', 'moto', DSC_LACR, ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarAereo;  // M
begin
  Gerador.wGrupo('aereo', '#01');
  Gerador.wCampo(tcInt, '#02', 'nMinu   ', 09, 09, 0, CTe.Aereo.nMinu, '');
  Gerador.wCampo(tcStr, '#03', 'nOCA    ', 14, 14, 0, CTe.Aereo.nOCA, '');
  Gerador.wCampo(tcDat, '#04', 'dPrev   ', 10, 10, 0, CTe.Aereo.dPrev, '');
  Gerador.wCampo(tcStr, '#05', 'xLAgEmi ', 01, 20, 0, CTe.Aereo.xLAgEmi, '');
  Gerador.wCampo(tcStr, '#06', 'cIATA   ', 01, 14, 0, CTe.Aereo.cIATA, '');

  Gerador.wGrupo('tarifa', '#07');
  Gerador.wCampo(tcStr, 'M08', 'trecho ', 01, 07, 0, CTe.Aereo.tarifa.trecho, '');
  Gerador.wCampo(tcStr, '#08', 'CL     ', 01, 02, 0, CTe.Aereo.tarifa.CL, '');
  Gerador.wCampo(tcStr, '#09', 'cTar   ', 01, 04, 0, CTe.Aereo.tarifa.cTar, '');
  Gerador.wCampo(tcDe2, '#10', 'vTar   ', 01, 15, 0, CTe.Aereo.tarifa.vTar, '');
  Gerador.wGrupo('/tarifa');

  Gerador.wGrupo('/aereo');
end;

procedure TCTeW.GerarAquav;  // N
var
 i, i01: Integer;
begin
  Gerador.wGrupo('aquav', '#01');
  Gerador.wCampo(tcDe2, '#02', 'vPrest   ', 01, 15, 1, CTe.Aquav.vPrest, '');
  Gerador.wCampo(tcDe2, '#03', 'vAFRMM   ', 01, 15, 1, CTe.Aquav.vAFRMM, '');
  Gerador.wCampo(tcStr, '#04', 'nBooking ', 01, 10, 0, CTe.Aquav.nBooking, '');
  Gerador.wCampo(tcStr, '#05', 'nCtrl    ', 01, 10, 0, CTe.Aquav.nCtrl, '');
  Gerador.wCampo(tcStr, '#06', 'xNavio   ', 01, 60, 1, CTe.Aquav.xNavio, '');
  Gerador.wCampo(tcStr, '#09', 'nViag    ', 01, 10, 0, CTe.Aquav.nViag, '');
  Gerador.wCampo(tcStr, '#10', 'direc    ', 01, 01, 1, TpDirecaoToStr(CTe.Aquav.direc), '');
  Gerador.wCampo(tcStr, '#11', 'prtEmb   ', 01, 60, 0, CTe.Aquav.prtEmb, '');
  Gerador.wCampo(tcStr, '#12', 'prtTrans ', 01, 60, 0, CTe.Aquav.prtTrans, '');
  Gerador.wCampo(tcStr, '#13', 'prtDest  ', 01, 60, 0, CTe.Aquav.prtDest, '');
  Gerador.wCampo(tcStr, '#14', 'tpNav    ', 01, 01, 1, TpNavegacaoToStr(CTe.Aquav.tpNav), '');
  Gerador.wCampo(tcStr, '#15', 'irin     ', 01, 10, 1, CTe.Aquav.irin, '');

  for i := 0 to CTe.Aquav.Lacre.Count - 1 do
   begin
    Gerador.wGrupo('lacre', '#16');
    Gerador.wCampo(tcStr, '#17', 'nLacre ', 01, 20, 1, CTe.Aquav.Lacre.Items[i].nLacre, '');
    Gerador.wGrupo('/lacre');
   end;
  if CTe.Aquav.Lacre.Count > 3 then
   Gerador.wAlerta('#16', 'lacre', '', ERR_MSG_MAIOR_MAXIMO + '3');

  Gerador.wGrupo('/aquav');
end;

procedure TCTeW.GerarFerrov;  // O
begin
  Gerador.wGrupo('ferrov', '#01');
  Gerador.wCampo(tcStr, '#02', 'tpTraf ', 01, 01, 1, TpTrafegoToStr(CTe.Ferrov.tpTraf), '');
  Gerador.wCampo(tcStr, '#06', 'fluxo  ', 01, 10, 1, CTe.Ferrov.fluxo, '');
  Gerador.wCampo(tcStr, '#07', 'idTrem ', 01, 07, 0, CTe.Ferrov.idTrem, '');
  Gerador.wCampo(tcDe2, '#08', 'vFrete ', 01, 15, 1, CTe.Ferrov.vFrete, '');
  (**) GerarFerroSub;
  (**) GerarDCL;
  (**) GerardetVag;
  Gerador.wGrupo('/ferrov');
end;

procedure TCTeW.GerarFerroSub;
begin
  if (CTe.Ferrov.ferroSub.CNPJ <> '') or
     (CTe.Ferrov.ferroSub.xNome <> '') then
  begin
    Gerador.wGrupo('ferroSub', 'O06');
    Gerador.wCampoCNPJ('O07', CTe.Ferrov.ferroSub.CNPJ, CODIGO_BRASIL, True);
    Gerador.wCampo(tcStr, 'O08', 'cInt   ', 01, 10, 0, CTe.Ferrov.ferroSub.cInt, '');

    if CTe.Ferrov.ferroSub.IE <> ''
     then begin
      Gerador.wCampo(tcStr, 'O09', 'IE ', 02, 14, 1, SomenteNumeros(CTe.Ferrov.ferroSub.IE), DSC_IE);

      if (FOpcoes.ValidarInscricoes)
       then if not ValidarIE(CTe.Ferrov.ferroSub.IE, CTe.Ferrov.ferroSub.EnderFerro.UF) then
        Gerador.wAlerta('O09', 'IE', DSC_IE, ERR_MSG_INVALIDO);
     end;

    Gerador.wCampo(tcStr, 'O10', 'xNome  ', 01, 60, 1, CTe.Ferrov.ferroSub.xNome, DSC_XNOME);
    (**) GerarEnderFerro;
    Gerador.wGrupo('/ferroSub');
  end;
end;

procedure TCTeW.GerarEnderFerro;
var
  cMun: integer;
  xMun: string;
  xUF: string;
begin
  AjustarMunicipioUF(xUF, xMun, cMun, CODIGO_BRASIL,
                                      CTe.Ferrov.ferroSub.EnderFerro.UF,
                                      CTe.Ferrov.ferroSub.EnderFerro.xMun,
                                      CTe.Ferrov.ferroSub.EnderFerro.cMun);
  Gerador.wGrupo('enderFerro', '#14');
  Gerador.wCampo(tcStr, '#15', 'xLgr    ', 01, 255, 1, CTe.Ferrov.ferroSub.EnderFerro.xLgr, DSC_XLGR);
  Gerador.wCampo(tcStr, '#16', 'nro     ', 01, 60, 0, ExecutarAjusteTagNro(FOpcoes.FAjustarTagNro, CTe.Ferrov.ferroSub.EnderFerro.nro), DSC_NRO);
  Gerador.wCampo(tcStr, '#17', 'xCpl    ', 01, 60, 0, CTe.Ferrov.ferroSub.EnderFerro.xCpl, DSC_XCPL);
  Gerador.wCampo(tcStr, '#18', 'xBairro ', 01, 60, 0, CTe.Ferrov.ferroSub.EnderFerro.xBairro, DSC_XBAIRRO);
  Gerador.wCampo(tcInt, '#19', 'cMun    ', 07, 07, 1, cMun, DSC_CMUN);
  if not ValidarMunicipio(CTe.Ferrov.ferroSub.EnderFerro.cMun) then
    Gerador.wAlerta('#19', 'cMun', DSC_CMUN, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcStr, '#20', 'xMun    ', 01, 60, 1, xMun, DSC_XMUN);
  Gerador.wCampo(tcInt, '#21', 'CEP     ', 08, 08, 0, CTe.Ferrov.ferroSub.EnderFerro.CEP, DSC_CEP);
  Gerador.wCampo(tcStr, '#22', 'UF      ', 02, 02, 1, xUF, DSC_UF);
  if not ValidarUF(xUF) then
    Gerador.wAlerta('#22', 'UF', DSC_UF, ERR_MSG_INVALIDO);
  Gerador.wGrupo('/enderFerro');
end;

procedure TCTeW.GerarDCL;
var
 i, i01, i02: Integer;
begin
  for i := 0 to CTe.Ferrov.DCL.Count - 1 do
   begin
    Gerador.wGrupo('DCL', 'O20');
    Gerador.wCampo(tcStr, 'O21', 'serie   ', 01, 03, 1, CTe.Ferrov.DCL.Items[i].serie, '');
    Gerador.wCampo(tcStr, 'O22', 'nDCL    ', 01, 20, 1, CTe.Ferrov.DCL.Items[i].nDCL, '');
    Gerador.wCampo(tcDat, 'O23', 'dEmi    ', 10, 10, 1, CTe.Ferrov.DCL.Items[i].dEmi, '');
    Gerador.wCampo(tcInt, 'O24', 'qVag    ', 01, 05, 1, CTe.Ferrov.DCL.Items[i].qVag, '');
    Gerador.wCampo(tcDe2, 'O25', 'pCalc   ', 01, 15, 1, CTe.Ferrov.DCL.Items[i].pCalc, '');
    Gerador.wCampo(tcDe2, 'O26', 'vTar    ', 01, 15, 1, CTe.Ferrov.DCL.Items[i].vTar, '');
    Gerador.wCampo(tcDe2, 'O27', 'vFrete  ', 01, 15, 1, CTe.Ferrov.DCL.Items[i].vFrete, '');
    Gerador.wCampo(tcDe2, 'O28', 'vSAcess ', 01, 15, 1, CTe.Ferrov.DCL.Items[i].vSAcess, '');
    Gerador.wCampo(tcDe2, 'O28', 'vTServ  ', 01, 15, 1, CTe.Ferrov.DCL.Items[i].vTServ, '');
    Gerador.wCampo(tcStr, 'O29', 'idTrem  ', 01, 07, 0, CTe.Ferrov.DCL.Items[i].idTrem, '');

    for i01 := 0 to CTe.Ferrov.DCL.Items[i].detVagDCL.Count - 1 do
     begin
      Gerador.wGrupo('detVagDCL', 'O30');
      Gerador.wCampo(tcInt, 'O31', 'nVag   ', 01, 08, 1, CTe.Ferrov.DCL.Items[i].detVagDCL.Items[i01].nVag, '');
      Gerador.wCampo(tcDe2, 'O32', 'cap    ', 01, 05, 0, CTe.Ferrov.DCL.Items[i].detVagDCL.Items[i01].cap, '');
      Gerador.wCampo(tcStr, 'O33', 'tpVag  ', 03, 03, 0, CTe.Ferrov.DCL.Items[i].detVagDCL.Items[i01].tpVag, '');
      Gerador.wCampo(tcDe2, 'O34', 'pesoR  ', 01, 05, 1, CTe.Ferrov.DCL.Items[i].detVagDCL.Items[i01].pesoR, '');
      Gerador.wCampo(tcDe2, 'O35', 'pesoBC ', 01, 05, 1, CTe.Ferrov.DCL.Items[i].detVagDCL.Items[i01].pesoBC, '');

      for i02 := 0 to CTe.Ferrov.DCL.Items[i].detVagDCL[i01].lacDetVagDCL.Count - 1 do
       begin
        Gerador.wGrupo('lacDetVagDCL', 'O36');
        Gerador.wCampo(tcStr, 'O37', 'nLacre ', 01, 20, 1, CTe.Ferrov.DCL.Items[i].detVagDCL.Items[i01].lacDetVagDCL.Items[i02].nLacre, '');
        Gerador.wGrupo('/lacDetVagDCL');
       end;
      if CTe.Ferrov.DCL.Items[i].detVagDCL[i01].lacDetVagDCL.Count > 990 then
       Gerador.wAlerta('O36', 'lacDetVagDCL', '', ERR_MSG_MAIOR_MAXIMO + '990');

      for i02 := 0 to CTe.Ferrov.DCL.Items[i].detVagDCL[i01].contDCL.Count - 1 do
       begin
        Gerador.wGrupo('contDCL', 'O38');
        Gerador.wCampo(tcStr, 'O39', 'nCont ', 01, 20, 1, CTe.Ferrov.DCL.Items[i].detVagDCL.Items[i01].contDCL.Items[i02].nCont, '');
        Gerador.wCampo(tcDat, 'O40', 'dPrev ', 10, 10, 0, CTe.Ferrov.DCL.Items[i].detVagDCL.Items[i01].contDCL.Items[i02].dPrev, '');
        Gerador.wGrupo('/contDCL');
       end;
      if CTe.Ferrov.DCL.Items[i].detVagDCL[i01].contDCL.Count > 990 then
       Gerador.wAlerta('O38', 'contDCL', '', ERR_MSG_MAIOR_MAXIMO + '990');

      Gerador.wGrupo('/detVagDCL');
     end;
    if CTe.Ferrov.DCL.Items[i].detVagDCL.Count > 990 then
     Gerador.wAlerta('O30', 'detVagDCL', '', ERR_MSG_MAIOR_MAXIMO + '990');

    Gerador.wGrupo('/DCL');
   end;
  if CTe.Ferrov.DCL.Count > 990 then
   Gerador.wAlerta('O20', 'DCL', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerardetVag;
var
 i, i01: Integer;
begin
  for i := 0 to CTe.Ferrov.detVag.Count - 1 do
   begin
    Gerador.wGrupo('detVag', '#23');
    Gerador.wCampo(tcInt, '#24', 'nVag   ', 08, 08, 1, CTe.Ferrov.detVag.Items[i].nVag, '');
    Gerador.wCampo(tcDe2, '#25', 'cap    ', 01, 05, 0, CTe.Ferrov.detVag.Items[i].cap, '');
    Gerador.wCampo(tcStr, '#26', 'tpVag  ', 03, 03, 0, CTe.Ferrov.detVag.Items[i].tpVag, '');
    Gerador.wCampo(tcDe2, '#27', 'pesoR  ', 01, 05, 1, CTe.Ferrov.detVag.Items[i].pesoR, '');
    Gerador.wCampo(tcDe2, '#28', 'pesoBC ', 01, 05, 1, CTe.Ferrov.detVag.Items[i].pesoBC, '');

    for i01 := 0 to CTe.Ferrov.detVag.Items[i].lacDetVag.Count - 1 do
     begin
      Gerador.wGrupo('lacDetVag', '#29');
      Gerador.wCampo(tcStr, '#30', 'nLacre ', 01, 20, 1, CTe.Ferrov.detVag.Items[i].lacDetVag.Items[i01].nLacre, '');
      Gerador.wGrupo('/lacDetVag');
     end;

    if CTe.Ferrov.detVag.Items[i].lacDetVag.Count > 990 then
     Gerador.wAlerta('#29', 'lacDetVag', '', ERR_MSG_MAIOR_MAXIMO + '990');

    for i01 := 0 to CTe.Ferrov.detVag.Items[i].contVag.Count - 1 do
     begin
      Gerador.wGrupo('contVag', '#31');
      Gerador.wCampo(tcStr, '#32', 'nCont ', 01, 20, 1, CTe.Ferrov.detVag.Items[i].contVag.Items[i01].nCont, '');
      Gerador.wCampo(tcDat, '#33', 'dPrev ', 10, 10, 0, CTe.Ferrov.detVag.Items[i].contVag.Items[i01].dPrev, '');
      Gerador.wGrupo('/contVag');
     end;
    if CTe.Ferrov.detVag.Items[i].contVag.Count > 990 then
     Gerador.wAlerta('#31', 'contVag', '', ERR_MSG_MAIOR_MAXIMO + '990');

    Gerador.wGrupo('/detVag');
   end;
  if CTe.Ferrov.detVag.Count > 990 then
   Gerador.wAlerta('#23', 'detVag', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarDuto;  // P
begin
  Gerador.wGrupo('duto', '#01');
  Gerador.wCampo(tcDe6, '#02', 'vTar ', 01, 15, 0, CTe.duto.vTar, '');
  Gerador.wGrupo('/duto');
end;

procedure TCTeW.GerarPeri;  // Q
var
 i: Integer;
begin
  for i := 0 to CTe.peri.Count - 1 do
   begin
    Gerador.wGrupo('peri', '#315');
    Gerador.wCampo(tcStr, '#316', 'nONU       ', 01,  04, 1, CTe.peri.Items[i].nONU, '');
    Gerador.wCampo(tcStr, '#317', 'xNomeAE    ', 01, 150, 1, CTe.peri.Items[i].xNomeAE, '');
    Gerador.wCampo(tcStr, '#318', 'xClaRisco  ', 01,  40, 1, CTe.peri.Items[i].xClaRisco, '');
    Gerador.wCampo(tcStr, '#319', 'grEmb      ', 01,  06, 0, CTe.peri.Items[i].grEmb, '');
    Gerador.wCampo(tcStr, '#320', 'qTotProd   ', 01,  20, 1, CTe.peri.Items[i].qTotProd, '');
    Gerador.wCampo(tcStr, '#321', 'qVolTipo   ', 01,  60, 0, CTe.peri.Items[i].qVolTipo, '');
    Gerador.wCampo(tcStr, '#322', 'pontoFulgor', 01,  06, 0, CTe.peri.Items[i].pontoFulgor, '');
    Gerador.wGrupo('/peri');
   end;
  if CTe.peri.Count > 990 then
   Gerador.wAlerta('#315', 'peri', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarVeicNovos;  // R
var
 i: Integer;
begin
  for i := 0 to CTe.veicNovos.Count - 1 do
   begin
    Gerador.wGrupo('veicNovos', '#323');
    Gerador.wCampo(tcStr, '#324', 'chassi ', 17, 17, 1, CTe.veicNovos.Items[i].chassi, '');
    Gerador.wCampo(tcStr, '#325', 'cCor   ', 01, 04, 1, CTe.veicNovos.Items[i].cCor, '');
    Gerador.wCampo(tcStr, '#326', 'xCor   ', 01, 40, 1, CTe.veicNovos.Items[i].xCor, '');
    Gerador.wCampo(tcStr, '#327', 'cMod   ', 01, 06, 1, CTe.veicNovos.Items[i].cMod, '');
    Gerador.wCampo(tcDe2, '#328', 'vUnit  ', 01, 15, 1, CTe.veicNovos.Items[i].vUnit, '');
    Gerador.wCampo(tcDe2, '#329', 'vFrete ', 01, 15, 1, CTe.veicNovos.Items[i].vFrete, '');
    Gerador.wGrupo('/veicNovos');
   end;
  if CTe.veicNovos.Count > 990 then
   Gerador.wAlerta('#323', 'veicNovos', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

procedure TCTeW.GerarCobr;
begin
  if (Trim(CTe.Cobr.Fat.nFat) <> '') or (CTe.Cobr.Fat.vOrig > 0) or
     (CTe.Cobr.Fat.vDesc > 0) or (CTe.Cobr.Fat.vLiq > 0) or
     (CTe.Cobr.Dup.Count > 0) then
  begin
    Gerador.wGrupo('cobr', '#330');
    (**)GerarCobrFat;
    (**)GerarCobrDup;
    Gerador.wGrupo('/cobr');
  end;
end;

procedure TCTeW.GerarCobrFat;
begin
  if (Trim(CTe.Cobr.Fat.nFat) <> '') or (CTe.Cobr.Fat.vOrig > 0) or
     (CTe.Cobr.Fat.vDesc > 0) or (CTe.Cobr.Fat.vLiq > 0) then
  begin
    Gerador.wGrupo('fat', '#331');
    Gerador.wCampo(tcStr, '#332', 'nFat  ', 01, 60, 0, CTe.Cobr.Fat.nFat, DSC_NFAT);
    Gerador.wCampo(tcDe2, '#333', 'vOrig ', 01, 15, 0, CTe.Cobr.Fat.vOrig, DSC_VORIG);
    Gerador.wCampo(tcDe2, '#334', 'vDesc ', 01, 15, 0, CTe.Cobr.Fat.vDesc, DSC_VDESC);
    Gerador.wCampo(tcDe2, '#335', 'vLiq  ', 01, 15, 0, CTe.Cobr.Fat.vLiq, DSC_VLIQ);
    Gerador.wGrupo('/fat');
  end;
end;

procedure TCTeW.GerarCobrDup;
var
  i: integer;
begin
  for i := 0 to CTe.Cobr.Dup.Count - 1 do
  begin
    Gerador.wGrupo('dup', '#336');
    Gerador.wCampo(tcStr, '#337', 'nDup  ', 01, 60, 0, CTe.Cobr.Dup[i].nDup, DSC_NDUP);
    Gerador.wCampo(tcDat, '#338', 'dVenc ', 10, 10, 0, CTe.Cobr.Dup[i].dVenc, DSC_DVENC);
    Gerador.wCampo(tcDe2, '#339', 'vDup  ', 01, 15, 0, CTe.Cobr.Dup[i].vDup, DSC_VDUP);
    Gerador.wGrupo('/dup');
  end;
end;

procedure TCTeW.GerarInfCTeSub;  // S
begin
 if CTe.infCTeSub.chCte<>''
  then begin
   Gerador.wGrupo('infCteSub', '#340');
   Gerador.wCampo(tcEsp, '#341', 'chCte ', 44, 44, 1, SomenteNumeros(CTe.infCTeSub.chCte), DSC_CHCTE);
   // Incluido por Italo em 13/05/2012
   if SomenteNumeros(CTe.infCTeSub.chCte) <> '' then
    if not ValidarChave('NFe' + SomenteNumeros(CTe.infCTeSub.chCte)) then
     Gerador.wAlerta('#341', 'chCte', DSC_REFNFE, ERR_MSG_INVALIDO);

   if (CTe.infCTeSub.tomaNaoICMS.refCteAnu='')
    then begin
     Gerador.wGrupo('tomaICMS', '#342');
     if (CTe.infCTeSub.tomaICMS.refNFe<>'')
      then begin
       Gerador.wCampo(tcEsp, '#343', 'refNFe ', 44, 44, 1, SomenteNumeros(CTe.infCTeSub.tomaICMS.refNFe), DSC_CHCTE);
       // Incluido por Italo em 13/05/2012
       if SomenteNumeros(CTe.infCTeSub.tomaICMS.refNFe) <> '' then
        if not ValidarChave('NFe' + SomenteNumeros(CTe.infCTeSub.tomaICMS.refNFe)) then
         Gerador.wAlerta('#343', 'refNFe', DSC_REFNFE, ERR_MSG_INVALIDO);
      end
      else begin
       if (CTe.infCTeSub.tomaICMS.refNF.CNPJ<>'')
        then begin
         Gerador.wGrupo('refNF', '#344');
         Gerador.wCampoCNPJ('#345', CTe.infCTeSub.tomaICMS.refNF.CNPJ, CODIGO_BRASIL, True);
         Gerador.wCampo(tcStr, '#346', 'mod      ', 02, 02, 1, CTe.infCTeSub.tomaICMS.refNF.modelo, '');
         Gerador.wCampo(tcInt, '#347', 'serie    ', 01, 03, 1, CTe.infCTeSub.tomaICMS.refNF.serie, '');
         Gerador.wCampo(tcInt, '#348', 'subserie ', 01, 03, 0, CTe.infCTeSub.tomaICMS.refNF.subserie, '');
         Gerador.wCampo(tcInt, '#349', 'nro      ', 01, 06, 1, CTe.infCTeSub.tomaICMS.refNF.nro, '');
         Gerador.wCampo(tcDe2, '#350', 'valor    ', 01, 15, 1, CTe.infCTeSub.tomaICMS.refNF.valor, '');
         Gerador.wCampo(tcDat, '#351', 'dEmi     ', 10, 10, 1, CTe.infCTeSub.tomaICMS.refNF.dEmi, '');
         Gerador.wGrupo('/refNF');
        end
        else begin
         Gerador.wCampo(tcEsp, '#352', 'refCte   ', 44, 44, 1, SomenteNumeros(CTe.infCTeSub.tomaICMS.refCte), DSC_CHCTE);
         // Incluido por Italo em 13/05/2012
         if SomenteNumeros(CTe.infCTeSub.tomaICMS.refCte) <> '' then
          if not ValidarChave('NFe' + SomenteNumeros(CTe.infCTeSub.tomaICMS.refCte)) then
           Gerador.wAlerta('#352', 'refCte', DSC_REFNFE, ERR_MSG_INVALIDO);
        end;
      end;
     Gerador.wGrupo('/tomaICMS');
    end
    else begin
     Gerador.wGrupo('tomaNaoICMS', '#353');
     Gerador.wCampo(tcEsp, '#354', 'refCteAnu ', 44, 44, 1, SomenteNumeros(CTe.infCTeSub.tomaNaoICMS.refCteAnu), DSC_CHCTE);
     // Incluido por Italo em 13/05/2012
     if SomenteNumeros(CTe.infCTeSub.tomaNaoICMS.refCteAnu) <> '' then
      if not ValidarChave('NFe' + SomenteNumeros(CTe.infCTeSub.tomaNaoICMS.refCteAnu)) then
       Gerador.wAlerta('#354', 'refCteAnu', DSC_REFNFE, ERR_MSG_INVALIDO);
     Gerador.wGrupo('/tomaNaoICMS');
    end;
   Gerador.wGrupo('/infCteSub');
  end;
end;

procedure TCTeW.GerarInfCTeComp;
var
  i, i01: integer;
begin
  if (CTe.Ide.tpCTe = tcComplemento) then
  begin
    for i := 0 to CTe.InfCTeComp.Count - 1 do
    begin
      Gerador.wGrupo('infCteComp', '#355');
      Gerador.wCampo(tcEsp, '#356', 'chave   ', 44, 44, 1, SomenteNumeros(CTe.infCTeComp[i].Chave), DSC_CHCTE);
      // Incluido por Italo em 13/05/2012
      if SomenteNumeros(CTe.infCTeComp[i].Chave) <> '' then
       if not ValidarChave('NFe' + SomenteNumeros(CTe.infCTeComp[i].Chave)) then
        Gerador.wAlerta('#356', 'chave', DSC_REFNFE, ERR_MSG_INVALIDO);
      Gerador.wGrupo('vPresComp', '#357');
      Gerador.wCampo(tcDe2, '#358', 'vTPrest ', 01, 15, 1, CTe.infCTeComp[i].vPresComp.vTPrest, DSC_VTPREST);

      for i01 := 0 to CTe.InfCTeComp[i].vPresComp.compComp.Count - 1 do
      begin
        if (CTe.InfCTeComp[i].vPresComp.compComp[i01].xNome <> '') and
          (CTe.InfCTeComp[i].vPresComp.compComp[i01].vComp <> 0) then
          begin
            Gerador.wGrupo('compComp', '#359');
            Gerador.wCampo(tcStr, '#360', 'xNome ', 01, 15, 1, CTe.InfCTeComp[i].vPresComp.compComp[i01].xNome, DSC_XNOMEC);
            Gerador.wCampo(tcDe2, '#361', 'vComp ', 01, 15, 1, CTe.InfCTeComp[i].vPresComp.compComp[i01].vComp, DSC_VCOMP);
            Gerador.wGrupo('/compComp');
          end;
      end;

      Gerador.wGrupo('/vPresComp');

      (**)GerarImpComp(i);

      Gerador.wGrupo('/infCteComp');
    end;
    // Alterado de 10 para 1 conforme NT 2013/002
    if CTe.InfCTeComp.Count > 1 then
      Gerador.wAlerta('#355', 'infCteComp', '', ERR_MSG_MAIOR_MAXIMO + '1');
  end;
end;

procedure TCTeW.GerarImpComp(i: Integer);
begin
  Gerador.wGrupo('impComp', '#362');
  (**)GerarICMSComp(i);
  Gerador.wCampo(tcDe2, '#398', 'vTotTrib   ', 01, 15, 0, CTe.InfCTeComp[i].impComp.vTotTrib, DSC_VCOMP);
  Gerador.wCampo(tcStr, '#398', 'infAdFisco ', 01, 1000, 0, CTe.InfCTeComp[i].impComp.InfAdFisco, DSC_INFADFISCO);
  Gerador.wGrupo('/impComp');
end;

procedure TCTeW.GerarICMSComp(i: Integer);
begin
  Gerador.wGrupo('ICMSComp', '363');

  if CTe.InfCTeComp[i].impComp.ICMSComp.SituTrib = cst00 then
    (**)GerarCST00Comp(i)
  else if CTe.InfCTeComp[i].impComp.ICMSComp.SituTrib = cst20 then
    (**)GerarCST20Comp(i)
  else if ((CTe.InfCTeComp[i].impComp.ICMSComp.SituTrib = cst40) or
           (CTe.InfCTeComp[i].impComp.ICMSComp.SituTrib = cst41) or
           (CTe.InfCTeComp[i].impComp.ICMSComp.SituTrib = cst51)) then
    (**)GerarCST45Comp(i)
  else if CTe.InfCTeComp[i].impComp.ICMSComp.SituTrib = cst80 then
    (**)GerarCST80Comp(i)
  else if CTe.InfCTeComp[i].impComp.ICMSComp.SituTrib = cst81 then
    (**)GerarCST81Comp(i)
  else if CTe.InfCTeComp[i].impComp.ICMSComp.SituTrib = cst90 then
    (**)GerarCST90Comp(i);

  Gerador.wGrupo('/ICMSComp');
end;

procedure TCTeW.GerarCST00Comp(i: Integer);
begin
  Gerador.wGrupo('CST00', '#364');
  Gerador.wCampo(tcStr, '#365', 'CST   ', 02, 02, 1, CSTICMSTOStr(CTe.InfCTeComp[i].impComp.ICMSComp.CST00.CST), DSC_CST);
  Gerador.wCampo(tcDe2, '#366', 'vBC   ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST00.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#367', 'pICMS ', 01, 05, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST00.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#368', 'vICMS ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST00.vICMS, DSC_VICMS);
  Gerador.wGrupo('/CST00');
end;

procedure TCTeW.GerarCST20Comp(i: Integer);
begin
  Gerador.wGrupo('CST20', '#369');
  Gerador.wCampo(tcStr, '#370', 'CST    ', 02, 02, 1, CSTICMSTOStr(CTe.InfCTeComp[i].impComp.ICMSComp.CST20.CST), DSC_CST);
  Gerador.wCampo(tcDe2, '#371', 'pRedBC ', 01, 05, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST20.pRedBC, DSC_PREDBC);
  Gerador.wCampo(tcDe2, '#372', 'vBC    ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST20.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#373', 'pICMS  ', 01, 05, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST20.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#374', 'vICMS  ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST20.vICMS, DSC_VICMS);
  Gerador.wGrupo('/CST20');
end;

procedure TCTeW.GerarCST45Comp(i: Integer);
begin
  Gerador.wGrupo('CST45', '#375');
  Gerador.wCampo(tcStr, '#376', 'CST ', 02, 02, 1, CSTICMSTOStr(CTe.InfCTeComp[i].impComp.ICMSComp.CST45.CST), DSC_CST);
  Gerador.wGrupo('/CST45');
end;

procedure TCTeW.GerarCST80Comp(i: Integer);
begin
  Gerador.wGrupo('CST80', 'U23');
  Gerador.wCampo(tcStr, 'U24', 'CST    ', 02, 02, 1, CSTICMSTOStr(CTe.InfCTeComp[i].impComp.ICMSComp.CST80.CST), DSC_CST);
  Gerador.wCampo(tcDe2, 'U25', 'vBC    ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST80.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, 'U26', 'pICMS  ', 01, 05, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST80.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, 'U27', 'vICMS  ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST80.vICMS, DSC_VICMS);
  if CTe.InfCTeComp[i].impComp.ICMSComp.CST80.vCred > 0 then
   Gerador.wCampo(tcDe2, 'U28', 'vCred ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST80.vCred, DSC_VCRED);
  Gerador.wGrupo('/CST80');
end;

procedure TCTeW.GerarCST81Comp(i: Integer);
begin
  Gerador.wGrupo('CST81', 'U29');
  Gerador.wCampo(tcStr, 'U30', 'CST     ', 02, 02, 1, CSTICMSTOStr(CTe.InfCTeComp[i].impComp.ICMSComp.CST81.CST), DSC_CST);
  if CTe.InfCTeComp[i].impComp.ICMSComp.CST81.pRedBC > 0 then
   Gerador.wCampo(tcDe2, 'U31', 'pRedBC ', 01, 05, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST81.pRedBC, DSC_PREDBC);
  Gerador.wCampo(tcDe2, 'U32', 'vBC     ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST81.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, 'U33', 'pICMS   ', 01, 05, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST81.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, 'U34', 'vICMS   ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST81.vICMS, DSC_VICMS);
  Gerador.wGrupo('/CST81');
end;

procedure TCTeW.GerarCST90Comp(i: Integer);
begin
  Gerador.wGrupo('CST90', '#383');
  Gerador.wCampo(tcStr, '#384', 'CST      ', 02, 02, 1, CSTICMSTOStr(CTe.InfCTeComp[i].impComp.ICMSComp.CST90.CST), DSC_CST);
  if CTe.InfCTeComp[i].impComp.ICMSComp.CST90.pRedBC > 0 then
    Gerador.wCampo(tcDe2, '#385', 'pRedBC ', 01, 05, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST90.pRedBC, DSC_PREDBC);
  Gerador.wCampo(tcDe2, '#386', 'vBC      ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST90.vBC, DSC_VBC);
  Gerador.wCampo(tcDe2, '#387', 'pICMS    ', 01, 05, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST90.pICMS, DSC_PICMS);
  Gerador.wCampo(tcDe2, '#388', 'vICMS    ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST90.vICMS, DSC_VICMS);
  if CTe.InfCTeComp[i].impComp.ICMSComp.CST90.vCred > 0 then
    Gerador.wCampo(tcDe2, '#389', 'vCred  ', 01, 15, 1, CTe.InfCTeComp[i].impComp.ICMSComp.CST90.vCred, DSC_VCRED);
  Gerador.wGrupo('/CST90');
end;

procedure TCTeW.GerarInfCTeAnu;
begin
  if (CTe.Ide.tpCTe = tcAnulacao) then
  begin
    Gerador.wGrupo('infCteAnu', '#399');
    Gerador.wCampo(tcEsp, '#400', 'chCte ', 44, 44, 1, SomenteNumeros(CTe.InfCTeAnuEnt.chCTe), DSC_CHCTE);
    // Incluido por Italo em 13/05/2012
    if SomenteNumeros(CTe.InfCTeAnuEnt.chCTe) <> '' then
     if not ValidarChave('NFe' + SomenteNumeros(CTe.InfCTeAnuEnt.chCTe)) then
      Gerador.wAlerta('#400', 'chCte', DSC_REFNFE, ERR_MSG_INVALIDO);
    Gerador.wCampo(tcDat, '#401', 'dEmi  ', 10, 10, 1, CTe.InfCTeAnuEnt.dEmi, DSC_DEMI);
    Gerador.wGrupo('/infCteAnu');
  end;
end;

procedure TCTeW.AjustarMunicipioUF(var xUF, xMun: string;
  var cMun: integer; cPais: integer; vxUF, vxMun: string; vcMun: integer);
var
  PaisBrasil: boolean;
begin
  PaisBrasil := cPais = CODIGO_BRASIL;
  cMun := IIf(PaisBrasil, vcMun, CMUN_EXTERIOR);
  xMun := IIf(PaisBrasil, vxMun, XMUN_EXTERIOR);
  xUF := IIf(PaisBrasil, vxUF, UF_EXTERIOR);
  xMun := ObterNomeMunicipio(xMun, xUF, cMun);
end;

function TCTeW.ObterNomeMunicipio(const xMun, xUF: string;
  const cMun: integer): string;
var
  i: integer;
  PathArquivo, Codigo: string;
  List: TstringList;
begin
  result := '';
  if (FOpcoes.NormatizarMunicipios) and (cMun <> CMUN_EXTERIOR) then
  begin
    PathArquivo := FOpcoes.FPathArquivoMunicipios + 'MunIBGE-UF' + InttoStr(UFparaCodigo(xUF)) + '.txt';
    if FileExists(PathArquivo) then
    begin
      List := TstringList.Create;
      List.LoadFromFile(PathArquivo);
      Codigo := IntToStr(cMun);
      i := 0;
      while (i < list.count) and (result = '') do
      begin
        if pos(Codigo, List[i]) > 0 then
          result := Trim(stringReplace(list[i], codigo, '', []));
        inc(i);
      end;
      List.free;
    end;
  end;
  if result = '' then
    result := xMun;
end;
