////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org/nfe                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
// Desenvolvimento                                                            //
//         de Cte: Wiliam Zacarias da Silva Rosa                              //
//                                                                            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcnGerador;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnAuxiliar, pcnConversao;

type

  TGeradorOpcoes = class;

  { TGerador }

  TGerador = class(TPersistent)
  private
    FArquivoFormatoXML: String;
    FArquivoFormatoTXT: String;
    FLayoutArquivoTXT: TstringList;
    FListaDeAlertas: TStringList;
    FTagNivel: string;
    FIDNivel: string;
    FOpcoes: TGeradorOpcoes;
    FPrefixo: string;

    procedure addStringArquivoXML(const Value: String);
  public
    FIgnorarTagNivel: string;
    FIgnorarTagIdentacao: string;

    constructor Create;
    destructor Destroy; override;

    function SalvarArquivo(const CaminhoArquivo: string; const FormatoGravacao: TpcnFormatoGravacao = fgXML): Boolean;
    procedure wGrupo(const TAG: string; const ID: string = ''; const Identar: Boolean = True);
    procedure wCampo(const Tipo: TpcnTipoCampo; ID, TAG: string; const min, max, ocorrencias: smallint; const valor: variant; const Descricao: string = ''; ParseTextoXML: Boolean = True; Atributo: String = '');
    procedure wCampoCNPJCPF(const ID1, ID2: string; CNPJCPF: string; obrigatorio: Boolean = True; PreencheZeros: Boolean = True);
    procedure wCampoCNPJ(const ID: string; CNPJ: string; const cPais: Integer; obrigatorio: Boolean);
    procedure wCampoCPF(const ID: string; CPF: string; const cPais: Integer; obrigatorio: Boolean);
    procedure wAlerta(const ID, TAG, Descricao, Alerta: string);
    procedure wTexto(const Texto: string);
    procedure gtNivel(ID: string);
    procedure gtCampo(const Tag, ConteudoProcessado: string);
    procedure gtAjustarRegistros(const ID: string);
  published
    property ArquivoFormatoXML: String read FArquivoFormatoXML write FArquivoFormatoXML;
    property ArquivoFormatoTXT: String read FArquivoFormatoTXT write FArquivoFormatoTXT;
    property IDNivel: string read FIDNivel write FIDNivel;
    property ListaDeAlertas: TStringList read FListaDeAlertas write FListaDeAlertas;
    property LayoutArquivoTXT: TStringList read FLayoutArquivoTXT write FLayoutArquivoTXT;
    property Opcoes: TGeradorOpcoes read FOpcoes write FOpcoes;
    property Prefixo: string read FPrefixo write FPrefixo;
  end;

  { TGeradorOpcoes }

  TGeradorOpcoes = class(TPersistent)
  private
    FDecimalChar: Char;
    FSomenteValidar: Boolean;
    FIdentarXML: Boolean;
    FRetirarEspacos: Boolean;
    FRetirarAcentos: Boolean;
    FNivelIdentacao: Integer;
    FTamanhoIdentacao: Integer;
    FSuprimirDecimais: Boolean;
    FTagVaziaNoFormatoResumido: Boolean;
    FFormatoAlerta: string;
    FQuebraLinha: String;
  public
    constructor Create;

  published
    property SomenteValidar: Boolean read FSomenteValidar write FSomenteValidar default False;
    property RetirarEspacos: Boolean read FRetirarEspacos write FRetirarEspacos default True;
    property RetirarAcentos: Boolean read FRetirarAcentos write FRetirarAcentos default True;
    property IdentarXML: Boolean read FIdentarXML write FIdentarXML default False;
    property TamanhoIdentacao: Integer read FTamanhoIdentacao write FTamanhoIdentacao default 3;
    property SuprimirDecimais: Boolean read FSuprimirDecimais write FSuprimirDecimais default False;
    property TagVaziaNoFormatoResumido: Boolean read FTagVaziaNoFormatoResumido write FTagVaziaNoFormatoResumido default True;
    property FormatoAlerta: string read FFormatoAlerta write FFormatoAlerta;
    property DecimalChar: Char read FDecimalChar write FDecimalChar default '.';
    property QuebraLinha: String read FQuebraLinha write FQuebraLinha;
  end;

const

  NAME_SPACE_GNRE = 'xmlns="http://www.gnre.pe.gov.br"';
  NAME_SPACE_CFE  = 'xmlns="http://www.fazenda.sp.gov.br/sat"';
  NAME_SPACE_SAT  = 'xmlns="http://www.fazenda.sp.gov.br/sat"';

  V0_02 = 'versao="0.02"';
  V1_00 = 'versao="1.00"';
  V1_01 = 'versao="1.01"';
  V1_02 = 'versao="1.02"';
  V1_03 = 'versao="1.03"';
  V1_04 = 'versao="1.04"';
  V1_07 = 'versao="1.07"';
  V1_10 = 'versao="1.10"';
  V2_00 = 'versao="2.00"';
  V2_01 = 'versao="2.01"';

implementation

uses
  DateUtils,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  pcnConsts;

{ TGeradorOpcoes }

constructor TGeradorOpcoes.Create;
begin
  inherited;

  FIdentarXML := False;
  FTamanhoIdentacao := 3;
  FFormatoAlerta := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'; // Vide comentário em wAlerta
  FRetirarEspacos := True;
  FRetirarAcentos := True;
  FSuprimirDecimais := False;
  FSomenteValidar := False;
  FTagVaziaNoFormatoResumido := True;
  FDecimalChar := '.';
end;

{ TGerador }

procedure TGerador.addStringArquivoXML(const Value: String);
begin
  FArquivoFormatoXML := FArquivoFormatoXML + Value;
end;

constructor TGerador.Create;
begin
  inherited;

  FOpcoes := TGeradorOpcoes.Create;
  FListaDeAlertas := TStringList.Create;
  FLayoutArquivoTXT := TStringList.Create;
  FPrefixo := '';
end;

destructor TGerador.Destroy;
begin
  FOpcoes.Free;
  FListaDeAlertas.Free;
  FLayoutArquivoTXT.Free;
  FIgnorarTagNivel := '!@#';
  FIgnorarTagIdentacao := '!@#';
  inherited Destroy;
end;

function TGerador.SalvarArquivo(const CaminhoArquivo: string; const FormatoGravacao: TpcnFormatoGravacao = fgXML): Boolean;
var
  ArquivoGerado: TStringList;
begin
  // Formato de gravação somente é válido para NFe
{$IFNDEF COMPILER23_UP}
  Result := False;
{$ENDIF}
  ArquivoGerado := TStringList.Create;
  try
    if FormatoGravacao = fgXML then
      ArquivoGerado.Add(FArquivoFormatoXML)
    else
      ArquivoGerado.Add(FArquivoFormatoTXT);

    ArquivoGerado.SaveToFile(CaminhoArquivo);
    Result := True;
  finally
    ArquivoGerado.Free;
  end;
end;

procedure TGerador.wAlerta(const ID, TAG, Descricao, Alerta: string);
var
  s: string;
begin
  // O Formato da mensagem de erro pode ser alterado pelo usuario alterando-se a property FFormatoAlerta: onde;
  // %TAGNIVEL%  : Representa o Nivel da TAG; ex: <transp><vol><lacres>
  // %TAG%       : Representa a TAG; ex: <nLacre>
  // %ID%        : Representa a ID da TAG; ex X34
  // %MSG%       : Representa a mensagem de alerta
  // %DESCRICAO% : Representa a Descrição da TAG
  s := FOpcoes.FFormatoAlerta;
  s := StringReplace(s, '%TAGNIVEL%', FTagNivel, [rfReplaceAll]);
  s := StringReplace(s, '%TAG%', TAG, [rfReplaceAll]);
  s := StringReplace(s, '%ID%', ID, [rfReplaceAll]);
  s := StringReplace(s, '%MSG%', Alerta, [rfReplaceAll]);
  s := StringReplace(s, '%DESCRICAO%', Trim(Descricao), [rfReplaceAll]);
  if Trim(Alerta) <> '' then
    FListaDeAlertas.Add(s);
end;

procedure TGerador.wGrupo(const TAG: string; const ID: string = ''; const Identar: Boolean = True);
var
  aTAG: string;
begin
  if copy(TAG, 1, 1) = '/' then
    aTAG := '/' + FPrefixo + copy(TAG, 2, length(TAG))
  else
    aTAG := FPrefixo + TAG;

  // A propriedade FIgnorarTagNivel é utilizada para Ignorar TAG
  // na construção dos níveis para apresentação na mensagem de erro.
  gtNivel(ID);
  // Caso a tag seja um Grupo com Atributo
  if (pos('="', aTAG) > 0) or (pos('= "', aTAG) > 0) then
    gtCampo(RetornarConteudoEntre(aTAG, ' ', '='), RetornarConteudoEntre(aTAG, '"', '"'));
  //
  if not SubStrEmSubStr(aTAG, FIgnorarTagNivel) then
  begin
    if aTAG[1] <> '/' then
      FTagNivel := FTagNivel + '<' + aTAG + '>';

    if (aTAG[1] = '/') and (Copy(aTAG, 2, 3) = 'det') then
      FTagNivel := Copy(FTagNivel, 1, pos('<det', FTagNivel) - 1)
    else
      FTagNivel := StringReplace(FTagNivel, '<' + Copy(aTAG, 2, MaxInt) + '>', '', []);
  end;

  if (Identar) and (aTAG[1] = '/') then
    Dec(FOpcoes.FNivelIdentacao);

  if SubStrEmSubStr(aTAG, FIgnorarTagIdentacao) then
    Dec(FOpcoes.FNivelIdentacao);

  if FOpcoes.IdentarXML then
    addStringArquivoXML(StringOfChar(' ', FOpcoes.FTamanhoIdentacao * FOpcoes.FNivelIdentacao) + '<' + aTAG + '>' + #13#10)
  else
    addStringArquivoXML('<' + aTAG + '>');

  if (Identar) and (aTAG[1] <> '/') then
    Inc(FOpcoes.FNivelIdentacao);
end;

procedure TGerador.wCampoCNPJCPF(const ID1, ID2: string; CNPJCPF: string;
  obrigatorio: Boolean; PreencheZeros: Boolean);
var
  Tamanho: Integer;
  Ocorrencia: Integer;
begin
  CNPJCPF    := OnlyNumber(trim(CNPJCPF));
  Tamanho    := length(CNPJCPF);
  Ocorrencia := Integer(obrigatorio);

  if (Tamanho <= 11) and (Tamanho > 0) then    // Se Vazio dá preferencia a CNPJ
  begin
    if PreencheZeros and (Tamanho <> 11) then
    begin
      CNPJCPF := PadLeft(CNPJCPF, 11, '0');
      Tamanho := 11;
    end;

    wCampo(tcStr, ID2, 'CPF  ', 0, 11, Ocorrencia, CNPJCPF);
    if not ValidarCPF(CNPJCPF) then
      wAlerta(ID2, 'CPF', 'CPF', ERR_MSG_INVALIDO);
  end
  else
  begin
    if PreencheZeros and (obrigatorio or (Tamanho > 0)) and (Tamanho <> 14) then
    begin
      CNPJCPF := PadLeft(CNPJCPF, 14, '0');
      Tamanho := 14;
    end;

    wCampo(tcStr, ID1, 'CNPJ', 0, 14, Ocorrencia, CNPJCPF);
    if (Tamanho > 0) and (not ValidarCNPJ(CNPJCPF)) then
      wAlerta(ID1, 'CNPJ', 'CNPJ', ERR_MSG_INVALIDO);
  end;

  if (not(Tamanho in [0, 11, 14])) then
    wAlerta(ID1 + '-' + ID2, 'CNPJ-CPF', 'CNPJ/CPF', ERR_MSG_INVALIDO);
end;

procedure TGerador.wCampoCNPJ(const ID: string; CNPJ: string; const cPais: Integer; obrigatorio: Boolean);
begin
  if cPais <> 1058 then
  begin
    wCampo(tcStr, ID, 'CNPJ', 00, 00, 1, '');
    exit;
  end;
  CNPJ := OnlyNumber(Trim(CNPJ));
  if obrigatorio then
    wCampo(tcEsp, ID, 'CNPJ', 14, 14, 1, CNPJ, DSC_CNPJ)
  else
    wCampo(tcEsp, ID, 'CNPJ', 14, 14, 0, CNPJ, DSC_CNPJ);
  if not ValidarCNPJ(CNPJ) then
    wAlerta(ID, 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
end;

procedure TGerador.wCampoCPF(const ID: string; CPF: string; const cPais: Integer; obrigatorio: Boolean);
begin
  if cPais <> 1058 then
  begin
    wCampo(tcStr, ID, 'CPF', 00, 00, 1, '');
    exit;
  end;
  CPF := OnlyNumber(Trim(CPF));
  if obrigatorio then
    wCampo(tcEsp, ID, 'CPF', 11, 11, 1, CPF, DSC_CPF)
  else
    wCampo(tcEsp, ID, 'CPF', 11, 11, 0, CPF, DSC_CPF);
  if not ValidarCPF(CPF) then
    wAlerta(ID, 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
end;

procedure TGerador.wCampo(const Tipo: TpcnTipoCampo; ID, TAG: string; const min, max, ocorrencias: smallint; const valor: variant; const Descricao: string = ''; ParseTextoXML: Boolean = True; Atributo: String = '');

  function IsEmptyDate(wAno, wMes, wDia: Word): Boolean;
  begin
    Result := ((wAno = 1899) and (wMes = 12) and (wDia = 30));
  end;

var
  NumeroDecimais: smallint;
  TamMin, TamMax: Integer;
  valorDbl: Double;
  Alerta, ConteudoProcessado, ATag: string;
  wAno, wMes, wDia, wHor, wMin, wSeg, wMse: Word;
  EstaVazio: Boolean;
begin
  if ocorrencias < 0 then
    Exit;

  ID                  := Trim(ID);
  Tag                 := FPrefixo + Trim(TAG);
  Atributo            := Trim(Atributo);
  EstaVazio           := False;
  NumeroDecimais      := 0;
  ConteudoProcessado  := '';
  TamMax              := max;
  TamMin              := min;

  case Tipo of
    tcStr:
      begin
        ConteudoProcessado := Trim(VarToStr(valor));
        EstaVazio := ConteudoProcessado = '';
      end;

    tcNumStr:
      begin
        ConteudoProcessado := Trim(VarToStr(valor));
        EstaVazio := ConteudoProcessado = '';
        
        if Length(ConteudoProcessado) < TamMin then
          ConteudoProcessado := PadLeft(ConteudoProcessado, TamMin, '0');
      end;

    tcStrOrig:
      begin
        ConteudoProcessado := VarToStr(valor);
        EstaVazio := ConteudoProcessado = '';
      end;

    tcDat, tcDatCFe:
      begin
        DecodeDate(VarToDateTime(valor), wAno, wMes, wDia);
        ConteudoProcessado := FormatFloat('0000', wAno) + '-' + FormatFloat('00', wMes) + '-' + FormatFloat('00', wDia);
        if Tipo = tcDatCFe then
          ConteudoProcessado := OnlyNumber(ConteudoProcessado);

        EstaVazio := IsEmptyDate(wAno, wMes, wDia);
      end;

    tcDatVcto:
      begin
        DecodeDate( VarToDateTime(valor), wAno, wMes, wDia);
        ConteudoProcessado := FormatFloat('00', wDia)+ '/' + FormatFloat('00', wMes)+ '/' +FormatFloat('0000', wAno);
        EstaVazio := IsEmptyDate( wAno, wMes, wDia );
      end;

    tcHor, tcHorCFe:
      begin
        DecodeTime( VarToDateTime(valor), wHor, wMin, wSeg, wMse);
        ConteudoProcessado := FormatFloat('00', wHor) + ':' + FormatFloat('00', wMin) + ':' + FormatFloat('00', wSeg);
        if Tipo = tcHorCFe then
          ConteudoProcessado := OnlyNumber(ConteudoProcessado);

        EstaVazio := (wHor = 0) and (wMin = 0) and (wSeg = 0);
      end;

    tcDatHor, tcDatHorCFe:
      begin
        DecodeDateTime( VarToDateTime(valor), wAno, wMes, wDia, wHor, wMin, wSeg, wMse);
        ConteudoProcessado := FormatFloat('0000', wAno) + '-' +
                              FormatFloat('00', wMes) + '-' +
                              FormatFloat('00', wDia) + 'T' +
                              FormatFloat('00', wHor) + ':' +
                              FormatFloat('00', wMin) + ':' +
                              FormatFloat('00', wSeg);
        if Tipo = tcDatHorCFe then
          ConteudoProcessado := OnlyNumber(ConteudoProcessado);

        EstaVazio := ((wAno = 1899) and (wMes = 12) and (wDia = 30));
      end;

    tcDe2, tcDe3, tcDe4, tcDe5, tcDe6, tcDe8, tcDe10, tcDe1:
      begin
        // adicionar um para que o máximo e mínimo não considerem a virgula
        if not FOpcoes.FSuprimirDecimais then
        begin
          TamMax := TamMax + 1;
          TamMin := TamMin + 1;
        end;
        
        // Tipo numerico com decimais
        case Tipo of
          tcDe1 : NumeroDecimais :=  1;
          tcDe2 : NumeroDecimais :=  2;
          tcDe3 : NumeroDecimais :=  3;
          tcDe4 : NumeroDecimais :=  4;
          tcDe5 : NumeroDecimais :=  5;
          tcDe6 : NumeroDecimais :=  6;
          tcDe8 : NumeroDecimais :=  8;
          tcDe10: NumeroDecimais := 10;
        end;

        try
          valorDbl := valor; // Converte Variant para Double
          ConteudoProcessado := FloatToString(valorDbl, FOpcoes.DecimalChar, FloatMask(NumeroDecimais, False));
        except
          valorDbl := 0;
          ConteudoProcessado := '0.00';
        end;

        EstaVazio := (valorDbl = 0) and (ocorrencias = 0);

        if StrToIntDef(Copy(ConteudoProcessado, pos(FOpcoes.DecimalChar, ConteudoProcessado) + NumeroDecimais + 1, 10),0) > 0 then
          walerta(ID, Tag, Descricao, ERR_MSG_MAXIMO_DECIMAIS + ' ' + IntToStr(NumeroDecimais));

        // Caso não seja um valor fracionário; retira os decimais.
        if FOpcoes.FSuprimirDecimais then
          if int(valorDbl) = valorDbl then
            ConteudoProcessado := IntToStr(Round(valorDbl));

        if Length(ConteudoProcessado) < TamMin then
          ConteudoProcessado := PadLeft(ConteudoProcessado, TamMin, '0');
      end;

    tcEsp:
      begin
        // Tipo String - somente numeros
        ConteudoProcessado := Trim(VarToStrDef(valor, ''));
        EstaVazio := (ConteudoProcessado = '');
        if not ValidarNumeros(ConteudoProcessado) then
          walerta(ID, Tag, Descricao, ERR_MSG_INVALIDO);
      end;

    tcInt, tcInt64:
      begin
        // Tipo Inteiro
        try
          if tipo = tcInt then
            ConteudoProcessado := IntToStr(StrToInt(VarToStr(valor)))
          else
            ConteudoProcessado := IntToStr(StrToInt64(VarToStr(valor)));
        except
          ConteudoProcessado := '0';
        end;

        EstaVazio := (ConteudoProcessado = '0') and (ocorrencias = 0);

        if Length(ConteudoProcessado) < TamMin then
          ConteudoProcessado := PadLeft(ConteudoProcessado, TamMin, '0');
      end;

     tcBoolStr:
       begin
         ConteudoProcessado := LowerCase(BoolToStr(valor, True));
         EstaVazio := ConteudoProcessado = '';
       end;
  end;

  alerta := '';
  //(Existem tags obrigatórias que podem ser nulas ex. cEAN)  if (ocorrencias = 1) and (EstaVazio) then
  if (ocorrencias = 1) and (EstaVazio) and (TamMin > 0) then
    alerta := ERR_MSG_VAZIO;

  if (length(ConteudoProcessado) < TamMin) and (Alerta = '') and (length(ConteudoProcessado) >= 1) then
    Alerta := ERR_MSG_MENOR;

  if length(ConteudoProcessado) > TamMax then
    Alerta := ERR_MSG_MAIOR;

  // Grava alerta //
  if (Alerta <> '') and (pos(ERR_MSG_VAZIO, Alerta) = 0) and (not EstaVazio) then
    Alerta := Alerta + ' [' + VarToStr(valor) + ']';

  walerta(ID, TAG, Descricao, alerta);
  // Sai se for apenas para validar //
  if FOpcoes.FSomenteValidar then
    exit;

  // Grava no Formato Texto
  if not EstaVazio then
    gtCampo(tag, ConteudoProcessado)
  else
    gtCampo(tag, '');

  // adiciona o espaço ao inicio do atributo para não colar na tag se nao estiver vazio
  if Atributo <> '' then
    Atributo := ' ' + Atributo;

  // Grava a tag no arquivo - Quando não existir algum conteúdo
  if ((ocorrencias = 1) and (EstaVazio)) then
  begin
    if FOpcoes.FIdentarXML then
    begin
      if FOpcoes.FTagVaziaNoFormatoResumido then
        addStringArquivoXML(StringOfChar(' ', FOpcoes.FTamanhoIdentacao * FOpcoes.FNivelIdentacao) + '<' + TAG + '/>' + #13#10)
      else
        addStringArquivoXML(StringOfChar(' ', FOpcoes.FTamanhoIdentacao * FOpcoes.FNivelIdentacao) + '<' + TAG + '></' + TAG + '>' + #13#10)
    end
    else
    begin
      if FOpcoes.FTagVaziaNoFormatoResumido then
        addStringArquivoXML('<' + tag + Atributo + '/>')
      else
        addStringArquivoXML('<' + tag + Atributo + '></' + tag + '>');
    end;
    exit;
  end;

  // Grava a tag no arquivo - Quando existir algum conteúdo
  if ((ocorrencias = 1) or (not EstaVazio)) then
  begin
    if ParseTextoXML then
      ATag := '<' + tag + Atributo +  '>' +
              FiltrarTextoXML(FOpcoes.FRetirarEspacos, ConteudoProcessado, FOpcoes.FRetirarAcentos, True, FOpcoes.FQuebraLinha) +
              '</' + tag + '>'
    else
      ATag := '<' + tag + Atributo +  '>' +
              ConteudoProcessado +
              '</' + tag + '>';

    if FOpcoes.FIdentarXML then
      addStringArquivoXML(
         StringOfChar(' ', FOpcoes.FTamanhoIdentacao * FOpcoes.FNivelIdentacao) +
         ATag + sLineBreak)
    else
      addStringArquivoXML(ATag);
  end;
end;

procedure TGerador.wTexto(const Texto: string);
begin
  addStringArquivoXML(Texto);
end;

// Gerador TXT

procedure TGerador.gtNivel(ID: string);
var
  i: Integer;
begin
  ID := UpperCase(ID);
  FIDNivel := ID;
  if (FLayoutArquivoTXT.Count = 0) or (ID = '') then
    exit;
  for i := 0 to FLayoutArquivoTXT.Count - 1 do
    if pos('<' + ID + '>', UpperCase(FLayoutArquivoTXT.Strings[i])) > 0 then
      FArquivoFormatoTXT := FArquivoFormatoTXT + FLayoutArquivoTXT.Strings[i] + #13;
end;

procedure TGerador.gtCampo(const Tag, ConteudoProcessado: string);
var
  i: Integer;
  List: TstringList;
begin
  if FLayoutArquivoTXT.Count = 0 then
    exit;
  List := TStringList.Create;
  try
    List.Text := FArquivoFormatoTXT;
    for i := 0 to List.count - 1 do
      if pos('<' + FIDNivel + '>', List.Strings[i]) > 0 then
        if pos('|' + UpperCase(Tag) + '¨', UpperCase(List.Strings[i])) > 0 then
          List[i] := StringReplace(List[i], '|' + UpperCase(Trim(TAG)) + '¨', '|' + conteudoProcessado, []);
    
    FArquivoFormatoTXT := List.Text;
  finally
    List.Free;
  end;
end;

procedure TGerador.gtAjustarRegistros(const ID: string);
var
  i, j, k: Integer;
  s, idLocal: string;
  ListArquivo: TstringList;
  ListCorrigido: TstringList;
  ListTAGs: TstringList;
begin
  if FLayoutArquivoTXT.Count = 0 then
    exit;
  ListTAGs := TStringList.Create;
  ListArquivo := TStringList.Create;
  ListCorrigido := TStringList.Create;
  try
    // Elimina registros não utilizados
    ListArquivo.Text := FArquivoFormatoTXT;
    for i := 0 to ListArquivo.Count - 1 do
    begin
      k := 0;
      for j := 0 to FLayoutArquivoTXT.count - 1 do
        if listArquivo[i] = FLayoutArquivoTXT[j] then
          if pos('¨', listArquivo[i]) > 0 then
            k := 1;

      if k = 0 then
        ListCorrigido.Add(ListArquivo[i]);
    end;

    // Insere dados da chave da Nfe
    for i := 0 to ListCorrigido.Count - 1 do
      if pos('^ID^', ListCorrigido[i]) > 1 then
        ListCorrigido[i] := StringReplace(ListCorrigido[i], '^ID^', ID, []);

    // Elimina Nome de TAG sem informação
    for j := 0 to FLayoutArquivoTXT.Count - 1 do
    begin
      s := FLayoutArquivoTXT[j];
      while (pos('|', s) > 0) and (pos('¨', s) > 0) do
      begin
        s := Copy(s, pos('|', s), MaxInt);
        ListTAGs.Add(Copy(s, 1, pos('¨', s)));
        s := Copy(s, pos('¨', s) + 1, MaxInt);
      end;
    end;

    for i := 0 to ListCorrigido.Count - 1 do
      for j := 0 to ListTAGs.Count - 1 do
        ListCorrigido[i] := StringReplace(ListCorrigido[i], ListTAGs[j], '|', []);

    // Elimina Bloco <ID>
    for i := 0 to ListCorrigido.Count - 1 do
      if pos('>', ListCorrigido[i]) > 0 then
      begin
        ListCorrigido[i] := Trim(copy(ListCorrigido[i], pos('>', ListCorrigido[i]) + 1, maxInt));
        idLocal := copy(ListCorrigido[i],1,pos('|',ListCorrigido[i])-1);

        if (length(idLocal) > 2) and (UpperCase(idLocal) <> 'NOTA FISCAL') and
           (copy(idLocal,length(idLocal),1) <> OnlyNumber(copy(idLocal,length(idLocal),1))) then
        begin
          idLocal := copy(idLocal,1,length(idLocal)-1)+LowerCase(copy(idLocal,length(idLocal),1));
          ListCorrigido[i] := StringReplace(ListCorrigido[i],idLocal,idLocal,[rfIgnoreCase]);
        end;
      end;

    FArquivoFormatoTXT := ListCorrigido.Text;
    //
  finally
    ListTAGs.Free;
    ListArquivo.Free;
    ListCorrigido.Free;
  end;
end;

end.

