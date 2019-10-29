{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibDistribuicaoDFe;

interface

uses
  Classes, SysUtils, contnrs,
  ACBrLibResposta,
  pcnConversao, pcnRetDistDFeInt;

type
  { TDistribuicaoDFeItemResposta }
  TDistribuicaoDFeItemResposta = class(TACBrLibRespostaBase)
  private
    Fversao: String;
    FtpAmb: String;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    Farquivo: String;
    FCNPJ: string;
    FCNPJDest: string;
    FcOrgao: integer;
    FcOrgaoAutor: integer;
    FcSitNFe: String;
    FcteChvCte: String;
    FcteDhemi: TDateTime;
    FcteDhRebcto: TDateTime;
    FcteModal: string;
    FcteNProt: string;
    FdescEvento: string;
    FdhEmi: TDateTime;
    FdhEvento: TDateTime;
    FdhRegEvento: TDateTime;
    FdigVal: String;
    FdhRecbto: TDateTime;
    FemailDest: string;
    FEmiCNPJ: string;
    FEmiIE: string;
    FEmixNome: string;
    FId: string;
    FIE: String;
    FnProt: String;
    FnSeqEvento: Integer;
    FNSU: string;
    FchNFe: string;
    FCNPJCPF: string;
    Fschema: String;
    FtpEvento: string;
    FtpNF: String;
    FverEvento: string;
    FvNF: Currency;
    FxEvento: string;
    FxJust: string;
    FXML: string;
    FxNome: string;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AresDFe: TresDFe; const ANSU, AArquivo, AXml: String; const  ASchema: TSchemaDFe); overload;
    procedure Processar(const AresEvento: TresEvento; const ANSU, AArquivo, AXml: String; const  ASchema: TSchemaDFe); overload;
    procedure Processar(const AprocEvento: TprocEvento; const ANSU, AArquivo, AXml: String; const  ASchema: TSchemaDFe); overload;
    procedure Processar(const ARetInfEvento: TprocEvento_RetInfEvento; const ANSU, AArquivo, AXml: String; const  ASchema: TSchemaDFe); overload;

  published
    property Versao: string read Fversao write Fversao;
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property NSU: string read FNSU write FNSU;
    property chNFe: string read FchNFe write FchNFe;
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property xNome: string read FxNome write FxNome;
    property IE: String read FIE write FIE;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;
    property tpNF: String read FtpNF write FtpNF;
    property vNF: Currency read FvNF write FvNF;
    property digVal: String read FdigVal write FdigVal;
    property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property cSitNFe: String read FcSitNFe write FcSitNFe;
    property nProt: String read FnProt write FnProt;
    property XML: string read FXML write FXML;
    property arquivo: String read Farquivo write Farquivo;
    property schema: String read Fschema write Fschema;
    property dhEvento: TDateTime read FdhEvento write FdhEvento;
    property tpEvento: string read FtpEvento write FtpEvento;
    property xEvento: string read FxEvento write FxEvento;
    property nSeqEvento: Integer read FnSeqEvento write FnSeqEvento;
    property cOrgao: integer read FcOrgao write FcOrgao;
    property CNPJ: string read FCNPJ write FCNPJ;
    property Id: string read FId write FId;
    property verEvento: string read FverEvento write FverEvento;
    property descEvento: string read FdescEvento write FdescEvento;
    property xJust: string read FxJust write FxJust;
    property xMotivo: string read FxMotivo write FxMotivo;
    property EmiCNPJ: string read FEmiCNPJ write FEmiCNPJ;
    property EmiIE: string read FEmiIE write FEmiIE;
    property EmixNome: string read FEmixNome write FEmixNome;
    property cteNProt: string read FcteNProt write FcteNProt;
    property cteChvCte: String read FcteChvCte write FcteChvCte;
    property cteDhemi: TDateTime read FcteDhemi write FcteDhemi;
    property cteDhRebcto: TDateTime read FcteDhRebcto write FcteDhRebcto;
    property cteModal: string read FcteModal write FcteModal;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property cOrgaoAutor: integer read FcOrgaoAutor write FcOrgaoAutor;
    property dhRegEvento: TDateTime read FdhRegEvento write FdhRegEvento;
    property emailDest: string read FemailDest write FemailDest;

    end;

  { TDistribuicaoDFeResposta }
  TDistribuicaoDFeResposta = class(TACBrLibRespostaBase)
  private
    FMsg: string;
    Fversao: string;
    FtpAmb: string;
    FverAplic: string;
    FcStat: integer;
    FxMotivo: string;
    FcUF: integer;
    FdhRecbto: TDateTime;
    Farquivo: string;
    FdhResp: TDateTime;
    FindCont: string;
    FmaxNSU: string;
    FultNSU: string;
    FItems: TObjectList;

    function GetItem(Index: Integer): TDistribuicaoDFeItemResposta;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const retDistDFeInt: TretDistDFeInt;
                        const msg, arquivo: String;
                        const ListaArqs: TStringList);
    function Gerar: String; override;

    property Items[Index: Integer]: TDistribuicaoDFeItemResposta read GetItem;

  published
    property Msg: string read FMsg write FMsg;
    property Versao: string read Fversao write Fversao;
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property CUF: integer read FcUF write FcUF;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property dhResp: TDateTime read FdhResp write FdhResp;
    property ultNSU: string read FultNSU write FultNSU;
    property maxNSU: string read FmaxNSU write FmaxNSU;
    property arquivo: string read Farquivo write Farquivo;
    property indCont: string read FindCont write FindCont;

  end;

implementation

uses
  pcnAuxiliar,
  ACBrUtil, ACBrLibConsts;

{ TDistribuicaoDFeItemResposta }

constructor TDistribuicaoDFeItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TDistribuicaoDFeItemResposta.Processar(const AresDFe: TresDFe; const ANSU, AArquivo, AXml: String;
  const  ASchema: TSchemaDFe);
begin
  NSU := ANSU;
  XML := AXml;
  Arquivo := AArquivo;
  schema := SchemaDFeToStr(ASchema);
  chNFe := AresDFe.chDFe;
  CNPJCPF := AresDFe.CNPJCPF;
  xNome := AresDFe.xNome;
  IE := AresDFe.IE;
  dhEmi := AresDFe.dhEmi;
  tpNF := tpNFToStr(AresDFe.tpNF);
  vNF := AresDFe.vNF;
  digVal := AresDFe.digVal;
  dhRecbto := AresDFe.dhRecbto;
  cSitNFe := SituacaoDFeToStr(AresDFe.cSitDFe);
  nProt := AresDFe.nProt;
end;

procedure TDistribuicaoDFeItemResposta.Processar(const AresEvento: TresEvento; const ANSU, AArquivo, AXml: String;
  const  ASchema: TSchemaDFe);
begin
  NSU := ANSU;
  XML := AXml;
  Arquivo := AArquivo;
  schema := SchemaDFeToStr(ASchema);
  chNFe := AresEvento.chDFe;
  CNPJCPF := AresEvento.CNPJCPF;
  dhEvento := AresEvento.dhEvento;
  tpEvento := TpEventoToStr(AresEvento.tpEvento);
  xEvento := AresEvento.xEvento;
  nSeqEvento := AresEvento.nSeqEvento;
  cOrgao := AresEvento.cOrgao;
  dhRecbto := AresEvento.dhRecbto;
  nProt := AresEvento.nProt;
end;

procedure TDistribuicaoDFeItemResposta.Processar(const AprocEvento: TprocEvento; const ANSU, AArquivo, AXml: String;
  const  ASchema: TSchemaDFe);
begin
  NSU := ANSU;
  XML := AXml;
  Arquivo := AArquivo;
  schema := SchemaDFeToStr(ASchema);
  chNFe := AprocEvento.chDFe;
  cOrgao := AprocEvento.cOrgao;
  CNPJ := AprocEvento.CNPJ;
  Id := AprocEvento.Id;
  dhEvento := AprocEvento.dhEvento;
  nSeqEvento := AprocEvento.nSeqEvento;
  tpAmb := TpAmbToStr(AprocEvento.tpAmb);
  tpEvento := TpEventoToStr(AprocEvento.tpEvento);
  verEvento := AprocEvento.verEvento;

  with AprocEvento.detEvento do
  begin
    descEvento := descEvento;
    xJust := xJust;
    xMotivo := xCorrecao;
    EmiCnpj := emit.CNPJ;
    EmiIE := emit.IE;
    EmixNome := emit.xNome;
    cteNProt := CTe.nProt;
    cteChvCte := CTe.chCTe;
    cteDhemi := CTe.dhEmi;
    cteModal := TpModalToStr(CTe.modal);
    cteDhRebcto := CTe.dhRecbto;
  end;
end;

procedure TDistribuicaoDFeItemResposta.Processar(const ARetInfEvento: TprocEvento_RetInfEvento; const ANSU, AArquivo,
  AXml: String; const  ASchema: TSchemaDFe);
begin
  NSU := ANSU;
  XML := AXml;
  Arquivo := AArquivo;
  schema := SchemaDFeToStr(ASchema);

  Id := ARetInfEvento.Id;
  VerAplic := ARetInfEvento.VerAplic;
  tpAmb := TpAmbToStr(ARetInfEvento.tpAmb);
  cOrgao := ARetInfEvento.cOrgao;
  chNFe := ARetInfEvento.chDFe;
  CStat := ARetInfEvento.cStat;
  CNPJDest := ARetInfEvento.CNPJDest;
  cOrgaoAutor := ARetInfEvento.cOrgaoAutor;
  tpEvento := TpEventoToStr(ARetInfEvento.tpEvento);
  nSeqEvento := ARetInfEvento.nSeqEvento;
  xEvento := ARetInfEvento.xEvento;
  XMotivo := ARetInfEvento.XMotivo;
  dhRegEvento := ARetInfEvento.dhRegEvento;
  emailDest :=ARetInfEvento. emailDest;
  nProt := ARetInfEvento.nProt;
end;

{ TDistribuicaoDFeResposta }

constructor TDistribuicaoDFeResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespDistribuicaoDFe, ATipo, AFormato);

  FItems := TObjectList.Create;
end;

destructor TDistribuicaoDFeResposta.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

function TDistribuicaoDFeResposta.Gerar: String;
Var
  i: Integer;
begin
  Result := Inherited Gerar;

  for i := 0 to FItems.Count - 1  do
  begin
    Result := Result + sLineBreak + TDistribuicaoDFeItemResposta(FItems.Items[i]).Gerar;
  end;
end;

function TDistribuicaoDFeResposta.GetItem(Index: Integer): TDistribuicaoDFeItemResposta;
begin
  Result := TDistribuicaoDFeItemResposta(FItems.Items[Index]);
end;

procedure TDistribuicaoDFeResposta.Processar(const retDistDFeInt: TretDistDFeInt; const msg, arquivo: String; const ListaArqs: TStringList);
var
  I, J: Integer;
  Item: TDistribuicaoDFeItemResposta;
begin
  self.Versao := retDistDFeInt.versao;
  self.Msg := msg;
  self.VerAplic := retDistDFeInt.VerAplic;
  self.tpAmb := TpAmbToStr(retDistDFeInt.tpAmb);
  self.CStat := retDistDFeInt.cStat;
  self.XMotivo := retDistDFeInt.XMotivo;
  self.dhResp := retDistDFeInt.dhResp;
  self.ultNSU := retDistDFeInt.ultNSU;
  self.maxNSU := retDistDFeInt.maxNSU;
  self.arquivo := arquivo;

  if retDistDFeInt.cStat = 137 then
    indCont := '1'  // Sim
  else
    indCont := '0'; // Não

  J := 1;
  for I := 0 to retDistDFeInt.docZip.Count - 1 do
  begin
    if (Trim(retDistDFeInt.docZip[I].resDFe.chDFe) <> '') then
    begin
      Item := TDistribuicaoDFeItemResposta.Create('ResNFe' + Trim(IntToStrZero(J, 3)), Tipo, FFormato);
      Item.Processar(retDistDFeInt.docZip.Items[I].resDFe,
                     retDistDFeInt.docZip.Items[I].NSU,
                     ListaArqs[I],
                     retDistDFeInt.docZip.Items[I].XML,
                     retDistDFeInt.docZip.Items[I].schema);
      FItems.Add(Item);
      inc(J);
    end;
  end;

  J := 1;
  for I := 0 to retDistDFeInt.docZip.Count - 1 do
  begin
    if (Trim(retDistDFeInt.docZip[I].resEvento.chDFe) <> '') then
    begin
      Item := TDistribuicaoDFeItemResposta.Create('ResEve' + Trim(IntToStrZero(J, 3)), Tipo, FFormato);
      Item.Processar(retDistDFeInt.docZip.Items[I].resEvento,
                     retDistDFeInt.docZip.Items[I].NSU,
                     ListaArqs[I],
                     retDistDFeInt.docZip.Items[I].XML,
                     retDistDFeInt.docZip.Items[I].schema);
         FItems.Add(Item);
         inc(J);
    end;
  end;

  J := 1;
  for I := 0 to retDistDFeInt.docZip.Count - 1 do
  begin
    if (Trim(retDistDFeInt.docZip[I].procEvento.detEvento.versao) <> '' ) then
    begin
      Item := TDistribuicaoDFeItemResposta.Create('ProEve' + Trim(IntToStrZero(J, 3)), Tipo, FFormato);
      Item.Processar(retDistDFeInt.docZip.Items[I].procEvento,
                     retDistDFeInt.docZip.Items[I].NSU,
                     listaArqs[I],
                     retDistDFeInt.docZip.Items[I].XML,
                     retDistDFeInt.docZip.Items[I].schema);
      FItems.Add(Item);
      inc(J);
    end;
  end;

  J := 1;
  for I := 0 to retDistDFeInt.docZip.Count - 1 do
  begin
    if (Trim(retDistDFeInt.docZip[I].procEvento.RetinfEvento.Id) <> '' ) then
    begin
      Item := TDistribuicaoDFeItemResposta.Create('InfEve' + Trim(IntToStrZero(J, 3)), Tipo, FFormato);
      Item.Processar(retDistDFeInt.docZip.Items[I].procEvento.RetinfEvento,
                     retDistDFeInt.docZip.Items[I].NSU,
                     listaArqs[I],
                     retDistDFeInt.docZip.Items[I].XML,
                     retDistDFeInt.docZip.Items[I].schema);
      FItems.Add(Item);
      inc(J);
    end;
  end;
end;

end.

