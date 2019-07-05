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

unit ACBrLibConsReciDFe;

interface

uses
  Classes, SysUtils, contnrs,
  ACBrLibResposta, pcnRetConsReciDFe;

type
  { TRetornoItemResposta }
  TRetornoItemResposta = class(TACBrLibResposta)
  private
    FId: String;
    FtpAmb: String;
    FverAplic: String;
    FchNFe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FXML: String;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);

    procedure Processar(const Item: TProtDFeCollectionItem);

  published
    property Id: String read FId write FId;
    property tpAmb: String read FtpAmb write FtpAmb;
    property verAplic: String read FverAplic write FverAplic;
    property chNFe: String read FchNFe write FchNFe;
    property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property nProt: String read FnProt write FnProt;
    property digVal: String read FdigVal write FdigVal;
    property cStat: Integer read FcStat write FcStat;
    property xMotivo: String read FxMotivo write FxMotivo;
    property XML: String read FXML write FXML;

  end;

  { TReciboResposta }
  TRetornoResposta = class(TACBrLibResposta)
  private
    FPrefix: string;
    FMsg: string;
    Fversao: string;
    FtpAmb: string;
    FverAplic: string;
    FcStat: integer;
    FxMotivo: string;
    FcUF: integer;
    FdhRecbto: TDateTime;
    FnRec: string;
    FcMsg: integer;
    FxMsg: string;
    FProtocolo: string;
    FChaveDFe: string;
    FItens: TObjectList;

    function GetItem(Index: Integer): TRetornoItemResposta;

  public
    constructor Create(const APrefix: String; const ATipo: TACBrLibRespostaTipo); reintroduce;
    destructor Destroy; override;

    procedure Processar(const RetConsReciDFe: TRetConsReciDFe; const Recibo, Msg, Protocolo, ChaveDFe: String);
    function Gerar: String; override;

    property Items[Index: Integer]: TRetornoItemResposta read GetItem; default;

  published
    property Msg: string read FMsg write FMsg;
    property Versao: string read Fversao write Fversao;
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property CUF: integer read FcUF write FcUF;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property nRec: string read FnRec write FnRec;
    property cMsg: integer read FcMsg write FcMsg;
    property xMsg: String read FxMsg write FxMsg;
    property Protocolo: String read FProtocolo write FProtocolo;
    property ChaveDFe: String read FChaveDFe write FChaveDFe;

  end;

  { TReciboResposta }
  TReciboResposta = class(TACBrLibResposta)
  private
    FPrefix: string;
    Fversao: string;
    FtpAmb: string;
    FverAplic: string;
    FcStat: integer;
    FxMotivo: string;
    FcUF: integer;
    FnRec: string;
    FItens: TObjectList;

    function GetItem(Index: Integer): TRetornoItemResposta;

  public
    constructor Create(const APrefix: String; const ATipo: TACBrLibRespostaTipo); reintroduce;
    destructor Destroy; override;

    procedure Processar(const RetConsReciDFe: TRetConsReciDFe; const Recibo: String);
    function Gerar: String; override;

    property Items[Index: Integer]: TRetornoItemResposta read GetItem; default;

  published
    property Versao: string read Fversao write Fversao;
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property CUF: integer read FcUF write FcUF;

  end;

implementation

uses
  pcnAuxiliar, pcnConversao,
  ACBrUtil,ACBrLibConsts;

{ TRetornoItemResposta }
constructor TRetornoItemResposta.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

procedure TRetornoItemResposta.Processar(const Item: TProtDFeCollectionItem);
begin
  FId := Item.Id;
  FtpAmb := TpAmbToStr(Item.tpAmb);
  FverAplic := Item.verAplic;
  FchNFe := Item.chDFe;
  FdhRecbto := Item.dhRecbto;
  FnProt := Item.nProt;
  FdigVal := Item.digVal;
  FcStat := Item.cStat;
  FxMotivo := Item.xMotivo;
  FXML := Item.XMLprotDFe;
end;

{ TRetornoResposta }
constructor TRetornoResposta.Create(const APrefix: String; const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespRetorno, ATipo);

  FPrefix := APrefix;
  FItens := TObjectList.Create(True);
end;

destructor TRetornoResposta.Destroy;
begin
  FItens.Free;

  Inherited Destroy;
end;

function TRetornoResposta.GetItem(Index: Integer): TRetornoItemResposta;
begin
  Result := TRetornoItemResposta(FItens[Index]);
end;

function TRetornoResposta.Gerar: String;
Var
  i: Integer;
begin
  Result := Inherited Gerar;
  for i := 0 to FItens.Count - 1 do
  begin
    Result := Result + sLineBreak + TRetornoItemResposta(FItens.Items[i]).Gerar;
  end;
end;

procedure TRetornoResposta.Processar(const RetConsReciDFe: TRetConsReciDFe; const Recibo, Msg, Protocolo, ChaveDFe: String);
Var
  i: Integer;
  Item: TRetornoItemResposta;
begin
  FVersao := RetConsReciDFe.versao;
  FTpAmb := TpAmbToStr(RetConsReciDFe.TpAmb);
  FverAplic := RetConsReciDFe.verAplic;
  FCStat := RetConsReciDFe.cStat;
  FXMotivo := RetConsReciDFe.xMotivo;
  FCUF := RetConsReciDFe.cUF;
  FnRec := Recibo;
  FcMsg := RetConsReciDFe.cMsg;
  FxMsg := RetConsReciDFe.xMsg;
  FMsg := Msg;
  FProtocolo := Protocolo;
  FChaveDFe := ChaveDFe;

  with RetConsReciDFe do
  begin
    for i := 0 to ProtDFe.Count - 1 do
    begin
      Item := TRetornoItemResposta.Create(FPrefix + Trim(IntToStr(StrToInt(copy(ProtDFe.Items[i].chDFe, 26, 9)))), Tipo);
      Item.Processar(ProtDFe.Items[i]);
      FItens.Add(Item);
    end;
  end;
end;

{ TReciboResposta }
constructor TReciboResposta.Create(const APrefix: String; const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespRetorno, ATipo);

  FPrefix := APrefix;
  FItens := TObjectList.Create(True);
end;

destructor TReciboResposta.Destroy;
begin
  FItens.Free;

  Inherited Destroy;
end;

function TReciboResposta.GetItem(Index: Integer): TRetornoItemResposta;
begin
  Result := TRetornoItemResposta(FItens[Index]);
end;

function TReciboResposta.Gerar: String;
Var
  i: Integer;
begin
  Result := Inherited Gerar;
  for i := 0 to FItens.Count - 1 do
  begin
    Result := Result + sLineBreak + TRetornoItemResposta(FItens.Items[i]).Gerar;
  end;
end;

procedure TReciboResposta.Processar(const RetConsReciDFe: TRetConsReciDFe; const Recibo: String);
Var
  i: Integer;
  Item: TRetornoItemResposta;
begin
  FVersao := RetConsReciDFe.Versao;
  FTpAmb := TpAmbToStr(RetConsReciDFe.TpAmb);
  FVerAplic := RetConsReciDFe.VerAplic;
  FnRec := Recibo;
  FCStat := RetConsReciDFe.cStat;
  FXMotivo := RetConsReciDFe.XMotivo;
  FCUF := RetConsReciDFe.cUF;

  with RetConsReciDFe do
  begin
    for i := 0 to ProtDFe.Count - 1 do
    begin
      Item := TRetornoItemResposta.Create(FPrefix + Trim(IntToStr(StrToInt(copy(ProtDFe.Items[i].chDFe, 26, 9)))), Tipo);
      Item.Processar(ProtDFe.Items[i]);
      FItens.Add(Item);
    end;
  end;
end;

end.

