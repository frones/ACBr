{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esse arquivo usa a classe  PCN (c) 2009 - Paulo Casagrande                  }
{  PCN - Projeto Cooperar NFe       (Found at URL:  www.projetocooperar.org)   }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit pcnCFeCanc;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnSignature;

type

  TCFeCanc = class;
  TInfCFe = class;
  Tide = class;
  TEmit = class;
  TenderEmit = class;
  TDest = class;
  TTotal = class;
  TInfAdic = class;
  TobsFiscoCollection = class;
  TobsFiscoCollectionItem = class;

  { TCFeTCFeCanc }

  { TCFeCanc }

  TCFeCanc = class
  private
    FIdentarXML: boolean;
    FinfCFe: TinfCFe;
    Fide: Tide;
    FEmit: TEmit;
    FDest: TDest;
    FNomeArquivo: String;
    FRetirarAcentos: boolean;
    FRetirarEspacos: boolean;
    FTamanhoIdentacao: integer;
    FTotal: TTotal;
    FInfAdic: TInfAdic;
    FSignature: TSignature;
    FXMLOriginal: AnsiString;

    function GetAsXMLString : AnsiString ;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear ;
    procedure ClearSessao ;

    function LoadFromFile(const AFileName : String): boolean;
    function SaveToFile(const AFileName : String): boolean;
    function GerarXML( ApenasTagsAplicacao: Boolean = false) : AnsiString ;
    procedure SetXMLString(const AValue : AnsiString) ;

    property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
    property AsXMLString : AnsiString read GetAsXMLString write SetXMLString ;
    property XMLOriginal: AnsiString read FXMLOriginal;
  published
    property infCFe: TinfCFe read FinfCFe write FinfCFe;
    property ide: Tide read Fide write Fide;
    property Emit: TEmit read FEmit write FEmit;
    property Dest: TDest read FDest write FDest;
    property Total: TTotal read FTotal write FTotal;
    property InfAdic: TInfAdic read FInfAdic write FInfAdic;
    property signature: Tsignature read Fsignature write Fsignature;

    property RetirarAcentos: boolean read FRetirarAcentos write FRetirarAcentos;
    property RetirarEspacos: boolean read FRetirarEspacos write FRetirarEspacos;
    property IdentarXML: boolean read FIdentarXML write FIdentarXML;
    property TamanhoIdentacao: integer read FTamanhoIdentacao write FTamanhoIdentacao;
  end;

  { TinfCFe }

  TinfCFe = class
  private
    Fversao : Real;
    FID: string;
    FchCanc: string;
    FdhEmi: TDateTime;
    function GetdEmi: TDateTime;
    function GethEmi: TDateTime;
    procedure SetdEmi(const Value: TDateTime);
    procedure SethEmi(const Value: TDateTime);
  public
    constructor Create;
    procedure Clear ;
  published
    property versao: Real read Fversao write Fversao;
    property ID: string read FID write FID;
    property chCanc: string read FchCanc write FchCanc;
    property dEmi: TDateTime read GetdEmi write SetdEmi;
    property hEmi: TDateTime read GethEmi write SethEmi;
  end;

  { Tide }

  Tide = class
  private
    FcUF: integer;
    FcNF: integer;
    Fmodelo: integer;
    FnserieSAT: integer;
    FnCFe: integer;
    FdhEmi: TDateTime;
    FcDV: integer;
    FCNPJ: string;
    FsignAC: string;
    FassinaturaQRCODE: string;
    FnumeroCaixa: integer;
    function GetdEmi : TDateTime ;
    function GethEmi : TDateTime ;
    procedure SetdEmi(AValue : TDateTime) ;
    procedure SethEmi(AValue : TDateTime) ;
  public
    constructor Create;
    procedure Clear ;
    procedure ClearSessao ;
  published
    property cUF: integer read FcUF write FcUF;
    property cNF: integer read FcNF write FcNF;
    property modelo: integer read Fmodelo write Fmodelo;
    property nserieSAT: integer read FnserieSAT write FnserieSAT;
    property nCFe: integer read FnCFe write FnCFe;
    property dEmi: TDateTime read GetdEmi write SetdEmi;
    property hEmi: TDateTime read GethEmi write SethEmi;
    property cDV: integer read FcDV write FcDV;
    property CNPJ: string read FCNPJ write FCNPJ;
    property signAC: string read FsignAC write FsignAC;
    property assinaturaQRCODE: string read FassinaturaQRCODE write FassinaturaQRCODE;
    property numeroCaixa: integer read FnumeroCaixa write FnumeroCaixa;
  end;

  { TEmit }

  TEmit = class
  private
    FCNPJ: string;
    FxNome: string;
    FxFant: string;
    FEnderEmit: TenderEmit;
    FIE: string;
    FIM: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  published
    property CNPJ: string read FCNPJ write FCNPJ;
    property xNome: string read FxNome write FxNome;
    property xFant: string read FxFant write FxFant;
    property EnderEmit: TEnderEmit read FEnderEmit write FEnderEmit;
    property IE: string read FIE write FIE ;
    property IM: string read FIM write FIM ;
  end;

  { TenderEmit }

  TenderEmit = class
  private
    FxLgr: string;
    Fnro: string;
    fxCpl: string;
    FxBairro: string;
    FxMun: string;
    FCEP: integer;
  public
    constructor Create;
    procedure Clear;
  published
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property xMun: string read FxMun write FxMun;
    property CEP: integer read FCEP write FCEP;
  end;

  { TDest }

  TDest = class
  private
    FCNPJCPF: string;
  public
    constructor Create;
    procedure Clear;
  published
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
  end;

  { TTotal }

  TTotal = class
  private
    FvCFe: Currency;
  public
    constructor Create(AOwner: TCFeCanc);
    destructor Destroy; override;
    procedure Clear;
  published
    property vCFe: Currency read FvCFe write FvCFe;
  end;

  { TInfAdic }

  TInfAdic = class
  private
    FobsFisco: TobsFiscoCollection;
    procedure SetobsFisco(Value: TobsFiscoCollection);
  public
    constructor Create(AOwner: TCFeCanc);
    destructor Destroy; override;
    procedure Clear;
  published
    property obsFisco: TobsFiscoCollection read FobsFisco write SetobsFisco;
  end;

  TobsFiscoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TobsFiscoCollectionItem;
    procedure SetItem(Index: Integer; Value: TobsFiscoCollectionItem);
  public
    constructor Create(AOwner: TinfAdic);
    function Add: TobsFiscoCollectionItem;
    property Items[Index: Integer]: TobsFiscoCollectionItem read GetItem write SetItem; default;
  end;

  TobsFiscoCollectionItem = class(TCollectionItem)
  private
    FxCampo: string;
    FxTexto: string;
  published
    property xCampo: string read FxCampo write FxCampo;
    property xTexto: string read FxTexto write FxTexto;
  end;


implementation

Uses dateutils,
  pcnCFeCancR, pcnCFeCancW,
  ACBrUtil;

{ TDest }

constructor TDest.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TDest.Clear ;
begin
  FCNPJCPF := '' ;
end ;

{ TenderEmit }

constructor TenderEmit.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TenderEmit.Clear ;
begin
  FxLgr   := '';
  Fnro    := '';
  fxCpl   := '';
  FxBairro:= '';
  FxMun   := '';
  FCEP    := 0 ;
end ;

{ Tide }

function Tide.GetdEmi : TDateTime ;
begin
  Result := DateOf( FdhEmi );
end;

function Tide.GethEmi : TDateTime ;
begin
  Result := TimeOf( FdhEmi );
end;

procedure Tide.SetdEmi(AValue : TDateTime) ;
begin
 FdhEmi := DateOf(AValue) + hEmi;
end;

procedure Tide.SethEmi(AValue : TDateTime) ;
begin
  FdhEmi := dEmi + TimeOf(AValue);
end;

constructor Tide.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure Tide.Clear ;
begin
  FcUF              := 0;
  Fmodelo           := 0;
  FnserieSAT        := 0 ;
  FCNPJ             := '';
  FsignAC           := '';
  FnumeroCaixa      := 0 ;
  ClearSessao;
end ;

procedure Tide.ClearSessao ;
begin
  FcNF              := 0;
  FnCFe             := 0;
  FdhEmi            := 0;
  FcDV              := 0;
  FassinaturaQRCODE := '';
end ;

{ TinfCFe }

constructor TinfCFe.Create ;
begin
  inherited ;
  Clear;
end ;

procedure TinfCFe.Clear ;
begin
  Fversao         := 0 ;
  FID             := '';
  FchCanc         := '';
end ;

function TinfCFe.GetdEmi: TDateTime;
begin
  Result := DateOf( FdhEmi );
end;

function TinfCFe.GethEmi: TDateTime;
begin
  Result := TimeOf( FdhEmi );
end;

procedure TinfCFe.SetdEmi(const Value: TDateTime);
begin
 FdhEmi := DateOf(Value) + hEmi;
end;

procedure TinfCFe.SethEmi(const Value: TDateTime);
begin
  FdhEmi := dEmi + TimeOf(Value);
end;

{ TobsFiscoCollection }

function TobsFiscoCollection.Add: TobsFiscoCollectionItem;
begin
  Result := TobsFiscoCollectionItem(inherited Add);
end;

constructor TobsFiscoCollection.Create(AOwner: TinfAdic);
begin
  inherited Create(TobsFiscoCollectionItem);
end;

function TobsFiscoCollection.GetItem(
  Index: Integer): TobsFiscoCollectionItem;
begin
  Result := TobsFiscoCollectionItem(inherited GetItem(Index));
end;

procedure TobsFiscoCollection.SetItem(Index: Integer;
  Value: TobsFiscoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfAdic }

constructor TInfAdic.Create(AOwner: TCFeCanc);
begin
  inherited Create;
  FobsFisco := TobsFiscoCollection.Create(Self);
  Clear;
end;

destructor TInfAdic.Destroy;
begin
  FobsFisco.Free;
  inherited;
end;

procedure TInfAdic.Clear ;
begin
  FobsFisco.Clear;
end ;

procedure TInfAdic.SetobsFisco(Value: TobsFiscoCollection);
begin
  FobsFisco.Assign(Value);
end;

{ TTotal }

constructor TTotal.Create(AOwner: TCFeCanc);
begin
  inherited Create;
end;

destructor TTotal.Destroy;
begin
  inherited;
end;

procedure TTotal.Clear ;
begin
  FvCFe         := 0;
end ;

{ TEmit }

constructor TEmit.Create;
begin
  inherited Create;
  FenderEmit := TenderEmit.Create;
end;

destructor TEmit.Destroy;
begin
  FEnderEmit.Free;
  inherited;
end;

procedure TEmit.Clear ;
begin
  FCNPJ  := '';
  FxNome    := '';
  FxFant    := '';
  FIE       := '' ;
  FIM       := '' ;
  FenderEmit.Clear;
end ;

{ TCFeTCFeCanc }

constructor TCFeCanc.Create;
begin
  FinfCFe  := TInfCFe.Create;
  FIde     := Tide.Create;
  FEmit    := TEmit.Create;
  FDest    := TDest.Create;
  FTotal   := TTotal.Create(self);
  FinfAdic := TinfAdic.Create(self);
  Fsignature := Tsignature.create;

  FRetirarAcentos := True;
  FRetirarEspacos := True;
  FIdentarXML := False;
  FTamanhoIdentacao := 3;

  Clear;
end;

destructor TCFeCanc.Destroy;
begin
  FinfCFe.Free;
  Fide.Free;
  FEmit.Free;
  FDest.Free;
  FTotal.Free;
  FinfAdic.Free;
  Fsignature.Free;
  inherited Destroy;
end;

procedure TCFeCanc.Clear ;
begin
  FinfCFe.Clear;
  Fide.Clear;
  FEmit.Clear;

  ClearSessao;
end ;

procedure TCFeCanc.ClearSessao ;
begin
  FXMLOriginal := '';
  FNomeArquivo := '';

  Fide.ClearSessao;
  FDest.Clear;
  FTotal.Clear;
  FInfAdic.Clear;
  FSignature.Clear;
end ;

function TCFeCanc.GetAsXMLString : AnsiString ;
begin
  if FXMLOriginal = '' then
    Result := GerarXML( false )
  else
    Result := FXMLOriginal;
end;

function TCFeCanc.GerarXML( ApenasTagsAplicacao: Boolean ): AnsiString;
var
  LocCFeCancW : TCFeCancW ;
begin
  LocCFeCancW := TCFeCancW.Create(Self);
  try
    LocCFeCancW.Gerador.Opcoes.RetirarAcentos := FRetirarAcentos;
    LocCFeCancW.Gerador.Opcoes.RetirarEspacos := FRetirarEspacos;
    LocCFeCancW.Gerador.Opcoes.IdentarXML := FIdentarXML;
    LocCFeCancW.Gerador.Opcoes.TamanhoIdentacao := FTamanhoIdentacao;

    LocCFeCancW.GerarXml( ApenasTagsAplicacao );
    FXMLOriginal := LocCFeCancW.Gerador.ArquivoFormatoXML;
  finally
    LocCFeCancW.Free;
  end ;

  Result := FXMLOriginal;
end;


procedure TCFeCanc.SetXMLString(const AValue : AnsiString) ;
var
 LocCFeCancR : TCFeCancR;
begin
  LocCFeCancR := TCFeCancR.Create(Self);
  try
    LocCFeCancR.Leitor.Arquivo := AValue;
    LocCFeCancR.LerXml;
  finally
    LocCFeCancR.Free
  end;

  FXMLOriginal := AValue;
end;

function TCFeCanc.LoadFromFile(const AFileName : String) : boolean ;
var
  SL : TStringList;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.LoadFromFile( AFileName );
    AsXMLString := SL.Text;
    FNomeArquivo := AFileName;
    Result := True;
  finally
    SL.Free;
  end;
end ;

function TCFeCanc.SaveToFile(const AFileName : String) : boolean ;
begin
  WriteToTXT(AFileName, AsXMLString, False, False);
  FNomeArquivo := AFileName;
  Result := True;
end ;

end.
 
