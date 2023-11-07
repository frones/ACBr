{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Claudemir Vitor Pereira                         }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrGNREGuias;

interface

uses
  Classes, 
  SysUtils, 
  StrUtils,
  ACBrDFeUtil, 
  pcnConversao, 
  pcnAuxiliar, 
  pcnLeitor,
  ACBrGNREConfiguracoes,
  pgnreGNRE, 
  pgnreGNRER, 
  pgnreGNREW;

type

  { Guia }

  Guia = class(TCollectionItem)
  private
    FGNRE: TGNRE;
    FGNREW: TGNREW;
    FGNRER: TGNRER;

    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;

    function GetConfirmada: Boolean;
    function GetProcessada: Boolean;
    function GetMsg: String;
    function GetNumID: String;
    function GetXMLAssinado: String;

    procedure SetXML(AValue: String);
    procedure SetXMLOriginal(AValue: String);
//    function ValidarConcatChave: Boolean;
    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;

    procedure Assinar;
    procedure Validar;

    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;
    function LerXML(AXML: String): Boolean;
    function LerArqIni(const AIniString: String): Boolean;
    function GerarGNReIni: String;
    function GerarXML: String;
    function GravarXML(NomeArquivo: String = ''; PathArquivo: String = ''): Boolean;
    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil);

    property NomeArq: String read FNomeArq write FNomeArq;
    property GNRE: TGNRE read FGNRE;
    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: String         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;
    property XMLAssinado: String read GetXMLAssinado write FXMLAssinado;
    property Confirmada: Boolean read GetConfirmada;
    property Processada: Boolean read GetProcessada;
    property Msg: String read GetMsg;
    property NumID: String read GetNumID;
    property Alertas: String read FAlertas;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property ErroRegrasdeNegocios: String read FErroRegrasdeNegocios;
  end;

  { TGuias }

  TGuias = class(TOwnedCollection)
  private
    FACBrGNRE: TComponent;
    FConfiguracoes: TConfiguracoesGNRE;

    function GetItem(Index: integer): Guia;
    procedure SetItem(Index: integer; const Value: Guia);

    procedure VerificarGNREGuias;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarGNRE;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;
    procedure Imprimir;
    procedure ImprimirPDF;

    function Add: Guia;
    function Insert(Index: integer): Guia;
    property Items[Index: integer]: Guia read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarGNRE que determina se após carregar os dados da GNRE
    // para o componente, será gerado ou não novamente o XML da GNRE.
    function LoadFromFile(CaminhoArquivo: String; AGerarGNRE: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarGNRE: Boolean = True): Boolean;
    function LoadFromString(AXMLString: String; AGerarGNRE: Boolean = True): Boolean;
    function LoadFromIni(AIniString: String): Boolean;
    function GerarIni: String;
    function GravarXML(PathNomeArquivo: String = ''): Boolean;

    property ACBrGNRE: TComponent read FACBrGNRE;
  end;

implementation

uses
  ACBrGNRE2,
  ACBrUtil.XMLHTML, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  pgnreConversao, synautil, IniFiles;

{ Guia }

constructor Guia.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);
  FGNRE := TGNRE.Create;
  FGNREW := TGNREW.Create(FGNRE);
  FGNRER := TGNRER.Create(FGNRE);
end;

destructor Guia.Destroy;
begin
  FGNREW.Free;
  FGNRER.Free;
  FGNRE.Free;
  inherited Destroy;
end;

procedure Guia.Imprimir;
begin
  with TACBrGNRE(TGuias(Collection).ACBrGNRE) do
  begin
    if not Assigned(GNREGuia) then
      raise EACBrGNREException.Create('Componente FGNREGuia não associado.')
    else
      GNREGuia.ImprimirGuia( nil {GuiasRetorno});
  end;
end;

procedure Guia.ImprimirPDF;
begin
  with TACBrGNRE(TGuias(Collection).ACBrGNRE) do
  begin
    if not Assigned(GNREGuia) then
      raise EACBrGNREException.Create('Componente FGNREGuia não associado.')
    else
      GNREGuia.ImprimirGuiaPDF( nil {GuiasRetorno});
  end;
end;

procedure Guia.Assinar;
var
  XMLStr: String;
  XMLUTF8: String;
  Leitor: TLeitor;
begin
  TACBrGNRE(TGuias(Collection).ACBrGNRE).SSL.ValidarCNPJCertificado( GNRE.c03_idContribuinteEmitente );

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrGNRE(TGuias(Collection).ACBrGNRE) do
  begin
    // FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'GNRE', 'infGNRE');
    FXMLAssinado := String(XMLUTF8);
    FXMLOriginal := FXMLAssinado;

    Leitor := TLeitor.Create;
    try
      leitor.Grupo := FXMLAssinado;
    finally
      Leitor.Free;
    end;

    if Configuracoes.Arquivos.Salvar and
       (not Configuracoes.Arquivos.SalvarApenasGNREProcessadas) then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLAssinado)
      else
        Gravar(CalcularNomeArquivoCompleto(), FXMLAssinado);
    end;
  end;
end;

procedure Guia.Validar;
var
  Erro, AXML: String;
  NotaEhValida: Boolean;
  ALayout: TLayOutGNRE;
  VerServ: Real;
begin
  AXML := XMLAssinado;

  with TACBrGNRE(TGuias(Collection).ACBrGNRE) do
  begin
    VerServ := VersaoGNREToDbl(Configuracoes.Geral.VersaoDF); // 1.00;

    ALayout := LayGNRERecepcao;

    // Extraindo apenas os dados da GNRE (sem GNREProc)
    AXML := '<GNRE xmlns' + RetornarConteudoEntre(AXML, '<GNRE xmlns', '</GNRE>') + '</GNRE>';

    NotaEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, VerServ), Erro);

    if not NotaEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados da nota: ') +
        IntToStr(GNRE.c02_receita) + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrGNREException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function Guia.VerificarAssinatura: Boolean;
var
  Erro, AXML: String;
  AssEhValida: Boolean;
begin
  AXML := XMLAssinado;

  with TACBrGNRE(TGuias(Collection).ACBrGNRE) do
  begin
    AssEhValida := SSL.VerificarAssinatura(AXML, Erro, 'infGNRE');

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura da nota: ') +
        IntToStr(GNRE.c02_receita) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function Guia.ValidarRegrasdeNegocios: Boolean;
begin
  Result := True; // Não Implementado
end;

function Guia.LerArqIni(const AIniString: String): Boolean;
var
  IniGuia: TMemIniFile;
  sSecao, sFIM: String;
  ok: Boolean;
  i: integer;
begin
  IniGuia := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, IniGuia);

    with FGNRe do
    begin
      sSecao := 'Emitente';

      if IniGuia.SectionExists(sSecao) then
      begin
        c27_tipoIdentificacaoEmitente := IniGuia.ReadInteger(sSecao,'tipo',0);//[1,2] INscrito na uf ou nao /////1 CNPJ - 2 CPF
        //Se inscrito na UF Destino
        c17_inscricaoEstadualEmitente := IniGuia.ReadString(sSecao,'IE','');  //IE inscrito na uf destino
        //Se nao Inscrito na uf Destino
        c03_idContribuinteEmitente    := IniGuia.ReadString(sSecao,'id','cnpjcpf'); //numero do cnpj ou cpf
        c16_razaoSocialEmitente       := IniGuia.ReadString(sSecao,'RazaoSocial','nome');
        c18_enderecoEmitente          := IniGuia.ReadString(sSecao,'Endereco','');
        c19_municipioEmitente         := IniGuia.ReadString(sSecao,'Cidade','');
        c20_ufEnderecoEmitente        := IniGuia.ReadString(sSecao,'UF','');
        c21_cepEmitente               := IniGuia.ReadString(sSecao,'Cep','');
        c22_telefoneEmitente          := IniGuia.ReadString(sSecao,'Telefone','');
      end;

      //Complementes da Recita
      sSecao := 'Complemento';

      if IniGuia.SectionExists(sSecao) then
      begin
        c42_identificadorGuia   := IniGuia.ReadString(sSecao,'IdentificadorGuia','');
        ///Exige Doc Origem
        c28_tipoDocOrigem       := IniGuia.ReadInteger(sSecao,'tipoDocOrigem',0);
        c04_docOrigem           := IniGuia.ReadString(sSecao,'DocOrigem','');
        ///Exige Detalhamento Receita
        c25_detalhamentoReceita := IniGuia.ReadInteger(sSecao,'detalhamentoReceita',0);
        ///Exige Produto
        c26_produto             := IniGuia.ReadInteger(sSecao,'produto',0);
      end;

      //Referencias Da Receita
      sSecao := 'Referencia';

      if IniGuia.SectionExists(sSecao) then
      begin
        tipoGNRE           := StrToTipoGNRE(ok, IniGuia.ReadString(sSecao,'tipoGNRe','0') );
        c15_convenio       := IniGuia.ReadString(sSecao,'convenio','');
        c02_receita        := IniGuia.ReadInteger(sSecao,'receita',0);
        c01_UfFavorecida   := IniGuia.ReadString(sSecao,'ufFavorecida','');
        c14_dataVencimento := StringToDateTime(IniGuia.ReadString(sSecao,'dataVencimento',''));
        c33_dataPagamento  := StringToDateTime(IniGuia.ReadString(sSecao,'dataPagamento',''));
        referencia.ano     := IniGuia.ReadInteger(sSecao,'referenciaAno',0);
        referencia.mes     := IniGuia.ReadString(sSecao,'referenciaMes','');
        referencia.parcela := IniGuia.ReadInteger(sSecao,'referenciaParcela',1);
        referencia.periodo := IniGuia.ReadInteger(sSecao,'referenciaPeriodo',0);
        c10_valorTotal     := StringToFloatDef(IniGuia.ReadString(sSecao,'ValorTotal',''),0);
        c06_valorPrincipal := StringToFloatDef(IniGuia.ReadString(sSecao,'ValorPrincipal',''),0);
        ValorFECP          := StringToFloatDef(IniGuia.ReadString(sSecao,'ValorFECP', ''), 0);
        TotalFECP          := StringToFloatDef(IniGuia.ReadString(sSecao,'TotalFECP', ''), 0);
        MultaFECP          := StringToFloatDef(IniGuia.ReadString(sSecao,'MultaFECP', ''), 0);
        JurosFECP          := StringToFloatDef(IniGuia.ReadString(sSecao,'JurosFECP', ''), 0);
        AtualMonetFECP     := StringToFloatDef(IniGuia.ReadString(sSecao,'AtualMonetFECP', ''), 0);
        MultaICMS          := StringToFloatDef(IniGuia.ReadString(sSecao,'MultaICMS', ''), 0);
        JurosICMS          := StringToFloatDef(IniGuia.ReadString(sSecao,'JurosICMS', ''), 0);
        AtualMonetICMS     := StringToFloatDef(IniGuia.ReadString(sSecao,'AtualMonetICMS', ''), 0);
      end;

      //Destinatario
      sSecao := 'Destinatario';

      if IniGuia.SectionExists(sSecao) then
      begin
        c34_tipoIdentificacaoDestinatario := IniGuia.ReadInteger(sSecao,'tipo',0);/// 1 CNPJ - 2 CPF
        //Se inscrito
        c36_inscricaoEstadualDestinatario := IniGuia.ReadString(sSecao,'ie','');
        //Se nao inscrito
        c35_idContribuinteDestinatario    := IniGuia.ReadString(sSecao,'id','cnpjcpf');
        c37_razaoSocialDestinatario       := IniGuia.ReadString(sSecao,'razaosocial','nome');
        c38_municipioDestinatario         := IniGuia.ReadString(sSecao,'cidade','');
      end;


      camposExtras.Clear;
      i := 1;
      while true do
      begin
        //Outras Informacoes
        sSecao := 'CampoExtra' + IntToStrZero(I, 1);

        sFIM := IniGuia.ReadString(sSecao, 'codigo', 'FIM');

        if (Length(sFIM) <= 0) or (sFIM = 'FIM') then
          break;

        with camposExtras.New do
        begin
          CampoExtra.codigo := StrToIntDef(sFIM, 0);
          CampoExtra.tipo   := IniGuia.ReadString(sSecao,'tipo','');
          CampoExtra.valor  := IniGuia.ReadString(sSecao,'valor','');
        end;

        Inc(i);
      end;

    end;

    GerarXML;

    Result := True;
  finally
    IniGuia.Free;
  end;
end;

function Guia.LerXML(AXML: String): Boolean;
begin
  XMLOriginal := AXML;
  FGNRER.Leitor.Arquivo := XMLOriginal;
  FGNRER.Versao := TACBrGNRE(TGuias(Collection).ACBrGNRE).Configuracoes.Geral.VersaoDF;

  Result := FGNRER.LerXML;
end;

function Guia.GravarXML(NomeArquivo, PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrGNRE(TGuias(Collection).ACBrGNRE).Gravar(FNomeArq, FXMLOriginal);
end;

function Guia.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));
  Result := True;
end;

procedure Guia.EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC, Anexos: TStrings);
var
  NomeArq : String;
  AnexosEmail:TStrings;
  StreamGNRE : TMemoryStream;
begin
  if not Assigned(TACBrGNRE(TGuias(Collection).ACBrGNRE).MAIL) then
    raise EACBrGNREException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamGNRE := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrGNRE(TGuias(Collection).ACBrGNRE) do
    begin
      GravarStream(StreamGNRE);

      if (EnviaPDF) then
      begin
        if Assigned(GNREGuia) then
        begin
          GNREGuia.ImprimirGuiaPDF( nil {GuiasRetorno});
          NomeArq := PathWithDelim(GNREGuia.PathPDF) + NumID + '-guia.pdf';
          AnexosEmail.Add(NomeArq);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamGNRE,
                   NumID + '-guia.xml');
    end;
  finally
    AnexosEmail.Free;
    StreamGNRE.Free;
  end;
end;

function Guia.GerarGNReIni: String;
var
  INIRec: TMemIniFile;
  IniGuia: TStringList;
  sSecao: String;
  i: Integer;
begin
  INIRec := TMemIniFile.Create('');
  try
    with FGNRe do
    begin
      sSecao := 'Emitente';
      INIRec.WriteInteger(sSecao, 'tipo', FGNRe.c27_tipoIdentificacaoEmitente);
      INIRec.WriteString(sSecao, 'IE', FGNRe.c17_inscricaoEstadualEmitente);
      INIRec.WriteString(sSecao, 'id', FGNRe.c03_idContribuinteEmitente);
      INIRec.WriteString(sSecao, 'RazaoSocial', FGNRe.c16_razaoSocialEmitente);
      INIRec.WriteString(sSecao, 'Endereco', FGNRe.c18_enderecoEmitente);
      INIRec.WriteString(sSecao, 'Cidade', FGNRe.c19_municipioEmitente);
      INIRec.WriteString(sSecao, 'UF', FGNRe.c20_ufEnderecoEmitente);
      INIRec.WriteString(sSecao, 'Cep', FGNRe.c21_cepEmitente);
      INIRec.WriteString(sSecao, 'Telefone', FGNRe.c22_telefoneEmitente);

      sSecao := 'Complemento';
      INIRec.WriteString(sSecao, 'IdentificadorGuia', FGNRe.c42_identificadorGuia);
      INIRec.WriteInteger(sSecao, 'tipoDocOrigem', FGNRe.c28_tipoDocOrigem);
      INIRec.WriteString(sSecao, 'DocOrigem', FGNRe.c04_docOrigem);
      INIRec.WriteInteger(sSecao, 'detalhamentoReceita', FGNRe.c25_detalhamentoReceita);
      INIRec.WriteInteger(sSecao, 'produto', FGNRe.c26_produto);

      sSecao := 'Referencia';
      INIRec.WriteString(sSecao, 'tipoGNRe', TipoGNREToStr(FGNRe.tipoGNRE));
      INIRec.WriteString(sSecao, 'convenio', FGNRe.c15_convenio);
      INIRec.WriteInteger(sSecao, 'receita', FGNRe.c02_receita);
      INIRec.WriteString(sSecao, 'ufFavorecida', FGNRe.c01_UfFavorecida);
      INIRec.WriteDateTime(sSecao, 'dataVencimento', FGNRe.c14_dataVencimento);
      INIRec.WriteDateTime(sSecao, 'dataPagamento', FGNRe.c33_dataPagamento);
      INIRec.WriteInteger(sSecao, 'referenciaAno', FGNRe.referencia.ano);
      INIRec.WriteString(sSecao, 'referenciaMes', FGNRe.referencia.mes);
      INIRec.WriteInteger(sSecao, 'referenciaPeriodo', FGNRe.referencia.periodo);
      INIRec.WriteFloat(sSecao, 'ValorTotal', FGNRe.c10_valorTotal);
      INIRec.WriteFloat(sSecao, 'ValorPrincipal', FGNRe.c06_valorPrincipal);
      INIRec.WriteFloat(sSecao, 'ValorFECP', FGNRe.ValorFECP);
      INIRec.WriteFloat(sSecao, 'TotalFECP', FGNRe.TotalFECP);
      INIRec.WriteFloat(sSecao, 'JurosFECP', FGNRe.JurosFECP);
      INIRec.WriteFloat(sSecao, 'AtualMonetFECP', FGNRe.AtualMonetFECP);
      INIRec.WriteFloat(sSecao, 'MultaICMS', FGNRe.MultaICMS);
      INIRec.WriteFloat(sSecao, 'JurosICMS', FGNRe.JurosICMS);
      INIRec.WriteFloat(sSecao, 'AtualMonetICMS', FGNRe.AtualMonetICMS);

      sSecao := 'Destinatario';
      INIRec.WriteInteger(sSecao, 'tipo', FGNRe.c34_tipoIdentificacaoDestinatario);
      INIRec.WriteString(sSecao, 'ie', FGNRe.c36_inscricaoEstadualDestinatario);
      INIRec.WriteString(sSecao, 'id', FGNRe.c35_idContribuinteDestinatario);
      INIRec.WriteString(sSecao, 'razaosocial', FGNRe.c37_razaoSocialDestinatario);
      INIRec.WriteString(sSecao, 'cidade', FGNRe.c38_municipioDestinatario);

      for i := 0 to FGNRe.camposExtras.Count - 1 do
      begin
        sSecao := 'CampoExtra' + IntToStrZero(i+1, 1);
        INIRec.WriteInteger(sSecao, 'codigo', FGNRe.camposExtras[i].CampoExtra.codigo);
        INIRec.WriteString(sSecao, 'tipo', FGNRe.camposExtras[i].CampoExtra.tipo);
        INIRec.WriteString(sSecao, 'valor', FGNRe.camposExtras[i].CampoExtra.valor);
      end;
    end;

    IniGuia := TStringList.Create;
    try
      IniRec.GetStrings(IniGuia);
      Result := StringReplace(IniGuia.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniGuia.Free;
    end;
  finally
    INIRec.Free;
  end;
end;

function Guia.GerarXML: String;
var
  IdAnterior : String;
begin
  with TACBrGNRE(TGuias(Collection).ACBrGNRE) do
  begin
    IdAnterior := GNRE.c42_identificadorGuia;
    FGNREW.Gerador.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FGNREW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FGNREW.Gerador.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FGNREW.Gerador.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;

    FGNREW.Versao := Configuracoes.Geral.VersaoDF;

    pcnAuxiliar.TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );
  end;

  FGNREW.GerarXml;
  //DEBUG
  //WriteToTXT('c:\temp\Guia.xml', FGNREW.Gerador.ArquivoFormatoXML, False, False);
  XMLOriginal := FGNREW.Gerador.ArquivoFormatoXML;

  // XML gerado pode ter nova Chave e ID, então devemos calcular novamente
  // o nome do arquivo, mantendo o PATH do arquivo carregado
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FGNRE.c42_identificadorGuia)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := ACBrStr( FGNREW.Gerador.ListaDeAlertas.Text );
  Result := FXMLOriginal;

  with TACBrGNRE(TGuias(Collection).ACBrGNRE) do
  begin
    if Configuracoes.Arquivos.Salvar then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLOriginal)
      else
        Gravar(CalcularNomeArquivoCompleto(), FXMLOriginal);
    end;
  end;
end;

function Guia.CalcularNomeArquivo: String;
var
  xID: String;
  NomeXML: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrGNREException.Create('ID Inválido. Impossível Salvar XML');

  NomeXML := '-gnre.xml';

  Result := xID + NomeXML;
end;

function Guia.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrGNRE(TGuias(Collection).ACBrGNRE) do
  begin
    if Configuracoes.Arquivos.EmissaoPathGNRE then
      Data := FGNRE.c14_dataVencimento
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathGNRE(Data, FGNRE.c03_idContribuinteEmitente, FGNRE.c17_inscricaoEstadualEmitente));
  end;
end;

function Guia.CalcularNomeArquivoCompleto(NomeArquivo,
  PathArquivo: String): String;
begin
  if EstaVazio(NomeArquivo) then
    NomeArquivo := CalcularNomeArquivo;

  if EstaVazio(PathArquivo) then
    PathArquivo := CalcularPathArquivo
  else
    PathArquivo := PathWithDelim(PathArquivo);

  Result := PathArquivo + NomeArquivo;
end;
(*
function Guia.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: word;
  chaveGNRE : String;
begin
  DecodeDate(GNRE.c14_dataVencimento, wAno, wMes, wDia);

  chaveGNRE := 'GNRE' + OnlyNumber(GNRE.c42_identificadorGuia);

  Result := True;
end;
*)
function Guia.GetConfirmada: Boolean;
begin
  Result := True;

//  Result := TACBrGNRE(TGuias(Collection).ACBrGNRE).CstatConfirmada(
//    FGNRE.procGNRE.cStat);
end;

function Guia.GetProcessada: Boolean;
begin
  result := True;
//  Result := TACBrGNRE(TGuias(Collection).ACBrGNRE).CstatProcessado(
//    FGNRE.procGNRE.cStat);
end;

function Guia.GetMsg: String;
begin
  Result := '';
//  Result := FGNRE.procGNRE.xMotivo;
end;

function Guia.GetNumID: String;
begin
  Result := OnlyNumber(GNRE.c42_identificadorGuia);
end;

function Guia.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure Guia.SetXML(AValue: String);
begin
  LerXML(AValue);
end;

procedure Guia.SetXMLOriginal(AValue: String);
begin
  FXMLOriginal := AValue;

  if XmlEstaAssinado(FXMLOriginal) then
    FXMLAssinado := FXMLOriginal
  else
    FXMLAssinado := '';
end;

{ TGuias }

constructor TGuias.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrGNRE) then
    raise EACBrGNREException.Create('AOwner deve ser do tipo TACBrGNRE');

  inherited;

  FACBrGNRE := TACBrGNRE(AOwner);
  FConfiguracoes := TACBrGNRE(FACBrGNRE).Configuracoes;
end;

function TGuias.Add: Guia;
begin
  Result := Guia(inherited Add);
end;

procedure TGuias.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

procedure TGuias.GerarGNRE;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TGuias.GerarIni: String;
begin
  Result := '';
  if (Self.Count > 0)then
    Result := Self.Items[0].GerarGNReIni;
end;

function TGuias.GetItem(Index: integer): Guia;
begin
  Result := Guia(inherited Items[Index]);
end;

function TGuias.GetNamePath: String;
begin
  Result := 'Guia';
end;

procedure TGuias.VerificarGNREGuias;
begin
  if not Assigned(TACBrGNRE(FACBrGNRE).GNREGuia) then
    raise EACBrGNREException.Create('Componente FGNREGuia não associado.');
end;

procedure TGuias.Imprimir;
begin
  VerificarGNREGuias;
  TACBrGNRE(FACBrGNRE).GNREGuia.ImprimirGuia(nil);
end;

procedure TGuias.ImprimirPDF;
begin
  VerificarGNREGuias;
  TACBrGNRE(FACBrGNRE).GNREGuia.ImprimirGuiaPDF(nil);
end;

function TGuias.Insert(Index: integer): Guia;
begin
  Result := Guia(inherited Insert(Index));
end;

procedure TGuias.SetItem(Index: integer; const Value: Guia);
begin
  Items[Index].Assign(Value);
end;

procedure TGuias.Validar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Validar;   // Dispara exception em caso de erro
end;

function TGuias.VerificarAssinatura(out Erros: String): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  for i := 0 to Self.Count - 1 do
  begin
    if not Self.Items[i].VerificarAssinatura then
    begin
      Result := False;
      Erros := Erros + Self.Items[i].ErroValidacao + sLineBreak;
    end;
  end;
end;

function TGuias.ValidarRegrasdeNegocios(out Erros: String): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  for i := 0 to Self.Count - 1 do
  begin
    if not Self.Items[i].ValidarRegrasdeNegocios then
    begin
      Result := False;
      Erros := Erros + Self.Items[i].ErroRegrasdeNegocios + sLineBreak;
    end;
  end;
end;

function TGuias.LoadFromFile(CaminhoArquivo: String; AGerarGNRE: Boolean): Boolean;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
  i, l: integer;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(CaminhoArquivo);
    XMLUTF8 := ReadStrFromStream(MS, MS.Size);
  finally
    MS.Free;
  end;

  l := Self.Count; // Indice da última guia já existente

  // Converte de UTF8 para a String nativa da IDE //
  XMLStr := DecodeToString(XMLUTF8, True);
  Result := LoadFromString(XMLStr, AGerarGNRE);

  if Result then
  begin
    // Atribui Nome do arquivo a novas guias inseridas //
    for i := l to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;

function TGuias.LoadFromIni(AIniString: String): Boolean;
begin
  with Self.Add do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TGuias.LoadFromStream(AStream: TStringStream;
  AGerarGNRE: Boolean): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarGNRE);
end;

function TGuias.LoadFromString(AXMLString: String;
  AGerarGNRE: Boolean): Boolean;
var
  AXML: String;
  P, N: integer;

  function PosGNRE: integer;
  begin
    Result := pos('</guias>', AXMLString);
  end;

begin
  N := PosGNRE;
  while N > 0 do
  begin
    P := pos('</GNREProc>', AXMLString);
    if P > 0 then
    begin
      AXML := copy(AXMLString, 1, P + 10);
      AXMLString := Trim(copy(AXMLString, P + 10, length(AXMLString)));
    end
    else
    begin
      AXML := copy(AXMLString, 1, N + 6);
      AXMLString := Trim(copy(AXMLString, N + 6, length(AXMLString)));
    end;

    with Self.Add do
    begin
      LerXML(AXML);

      if AGerarGNRE then // Recalcula o XML
        GerarXML;
    end;

    N := PosGNRE;
  end;

  Result := Self.Count > 0;
end;

function TGuias.GravarXML(PathNomeArquivo: String): Boolean;
var
  i: integer;
  NomeArq, PathArq : String;
begin
  Result := True;
  i := 0;
  while Result and (i < Self.Count) do
  begin
    PathArq := ExtractFilePath(PathNomeArquivo);
    NomeArq := ExtractFileName(PathNomeArquivo);
    Result := Self.Items[i].GravarXML(NomeArq, PathArq);
    Inc(i);
  end;
end;

end.
