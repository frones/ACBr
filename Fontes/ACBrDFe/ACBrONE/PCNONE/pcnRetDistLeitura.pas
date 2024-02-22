{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit pcnRetDistLeitura;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase, synacode,
  pcnConversao,
  pcnLeitor,
  ACBrONEConversao,
  pcnEnvRecepcaoLeitura;

type
  TinfMDFeCollectionItem = class(TObject)
  private
    FchMDFe: String;

  public
    property chMDFe: String read FchMDFe write FchMDFe;
  end;

  TinfMDFeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfMDFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfMDFeCollectionItem);
  public
    function New: TinfMDFeCollectionItem;
    property Items[Index: Integer]: TinfMDFeCollectionItem read GetItem write SetItem; default;
  end;

  TinfCompl = class(TObject)
  private
    FtpLeitura: TtpLeitura;
    FxEQP: String;
    Flatitude: Double;
    Flongitude: Double;
    Fplaca: String;
    FtpSentido: TtpSentido;
    FNSULeitura: String;
  public
    property tpLeitura: TtpLeitura read FtpLeitura  write FtpLeitura;
    property xEQP: String          read FxEQP       write FxEQP;
    property latitude: Double      read Flatitude   write Flatitude;
    property longitude: Double     read Flongitude  write Flongitude;
    property placa: String         read Fplaca      write Fplaca;
    property tpSentido: TtpSentido read FtpSentido  write FtpSentido;
    property NSULeitura: String    read FNSULeitura write FNSULeitura;
  end;

  TLeituraCollectionItem = class(TObject)
  private
    // Atributos
    FNSU: String;
    Fschema: TSchemaDFe;

    FXML: String;

    FRecepcaoLeitura: TRecepcaoLeitura;
    FinfMDFe: TinfMDFeCollection;
    FinfCompl: TinfCompl;
    procedure SetinfMDFe(const Value: TinfMDFeCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property NSU: String        read FNSU    write FNSU;
    property schema: TSchemaDFe read Fschema write Fschema;
    property XML: String        read FXML    write FXML;

    property RecepcaoLeitura: TRecepcaoLeitura read FRecepcaoLeitura write FRecepcaoLeitura;
    property infMDFe: TinfMDFeCollection read FinfMDFe  write SetinfMDFe;
    property infCompl: TinfCompl         read FinfCompl write FinfCompl;
  end;

  TLeituraCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TLeituraCollectionItem;
    procedure SetItem(Index: Integer; Value: TLeituraCollectionItem);
  public
    function New: TLeituraCollectionItem;
    property Items[Index: Integer]: TLeituraCollectionItem read GetItem write SetItem; default;
  end;

  TLeituraResumoCollectionItem = class(TObject)
  private
    // Atributos
    FNSU: String;
    Fschema: TSchemaDFe;

    FXML: String;

    FinfLeitura: TinfLeitura;
    FinfMDFe: TinfMDFeCollection;

    procedure SetinfMDFe(const Value: TinfMDFeCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property NSU: String        read FNSU    write FNSU;
    property schema: TSchemaDFe read Fschema write Fschema;
    property XML: String        read FXML    write FXML;

    property infLeitura: TinfLeitura     read FinfLeitura write FinfLeitura;
    property infMDFe: TinfMDFeCollection read FinfMDFe    write SetinfMDFe;
  end;

  TLeituraResumoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TLeituraResumoCollectionItem;
    procedure SetItem(Index: Integer; Value: TLeituraResumoCollectionItem);
  public
    function New: TLeituraResumoCollectionItem;
    property Items[Index: Integer]: TLeituraResumoCollectionItem read GetItem write SetItem; default;
  end;

  TLeituraCompactaCollectionItem = class(TObject)
  private
    // Atributos
    FNSU: String;
    Fschema: TSchemaDFe;

    FXML: String;

    FleituraComp: String;
    FinfMDFe: TinfMDFeCollection;

    procedure SetinfMDFe(const Value: TinfMDFeCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property NSU: String        read FNSU    write FNSU;
    property schema: TSchemaDFe read Fschema write Fschema;
    property XML: String        read FXML    write FXML;

    property leituraComp: String         read FleituraComp write FleituraComp;
    property infMDFe: TinfMDFeCollection read FinfMDFe     write SetinfMDFe;
  end;

  TLeituraCompactaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TLeituraCompactaCollectionItem;
    procedure SetItem(Index: Integer; Value: TLeituraCompactaCollectionItem);
  public
    function New: TLeituraCompactaCollectionItem;
    property Items[Index: Integer]: TLeituraCompactaCollectionItem read GetItem write SetItem; default;
  end;

  TRetDistLeitura = class
  private
    FLeitor: TLeitor;

    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FdhResp: TDateTime;
    FultNSU: String;
    FultNSUONE: String;

    FXML: String;

    FLeitura: TLeituraCollection;
    FleituraCompacta: TLeituraCompactaCollection;
    FleituraResumo: TLeituraResumoCollection;

    procedure SetLeitura(const Value: TLeituraCollection);
    procedure SetleituraCompacta(const Value: TLeituraCompactaCollection);
    procedure SetleituraResumo(const Value: TLeituraResumoCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
    function LerXMLFromFile(Const CaminhoArquivo: String): Boolean;

    property Leitor: TLeitor         read FLeitor    write FLeitor;
    property XML: String             read FXML       write FXML;
    property versao: String          read Fversao    write Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb     write FtpAmb;
    property verAplic: String        read FverAplic  write FverAplic;
    property cStat: Integer          read FcStat     write FcStat;
    property xMotivo: String         read FxMotivo   write FxMotivo;
    property dhResp: TDateTime       read FdhResp    write FdhResp;
    property ultNSU: String          read FultNSU    write FultNSU;
    property ultNSUONE: String       read FultNSUONE write FultNSUONE;

    property Leitura: TLeituraCollection read FLeitura   write SetLeitura;

    property leituraResumo: TLeituraResumoCollection read FleituraResumo   write SetleituraResumo;

    property leituraCompacta: TLeituraCompactaCollection read FleituraCompacta   write SetleituraCompacta;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TLeituraCollection }

function TLeituraCollection.GetItem(Index: Integer): TLeituraCollectionItem;
begin
  Result := TLeituraCollectionItem(inherited Items[Index]);
end;

procedure TLeituraCollection.SetItem(Index: Integer;
  Value: TLeituraCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TLeituraCollection.New: TLeituraCollectionItem;
begin
  Result := TLeituraCollectionItem.Create;
  Add(Result);
end;

{ TLeituraCollectionItem }

constructor TLeituraCollectionItem.Create;
begin
  inherited Create;

  FRecepcaoLeitura := TRecepcaoLeitura.Create;
  FinfMDFe := TinfMDFeCollection.Create();
  FinfCompl := TinfCompl.Create;
end;

destructor TLeituraCollectionItem.Destroy;
begin
  FRecepcaoLeitura.Free;
  FinfMDFe.Free;
  FinfCompl.Free;

  inherited;
end;

procedure TLeituraCollectionItem.SetinfMDFe(const Value: TinfMDFeCollection);
begin
  FinfMDFe := Value;
end;

{ TRetDistLeitura }

constructor TRetDistLeitura.Create;
begin
  inherited Create;

  FLeitor  := TLeitor.Create;
  FLeitura := TLeituraCollection.Create();
  FleituraResumo := TLeituraResumoCollection.Create();
  FleituraCompacta := TLeituraCompactaCollection.Create();
end;

destructor TRetDistLeitura.Destroy;
begin
  FLeitor.Free;
  FLeitura.Free;
  FleituraResumo.Free;
  FleituraCompacta.Free;

  inherited;
end;

procedure TRetDistLeitura.SetLeitura(const Value: TLeituraCollection);
begin
  FLeitura := Value;
end;

procedure TRetDistLeitura.SetleituraCompacta(
  const Value: TLeituraCompactaCollection);
begin
  FleituraCompacta := Value;
end;

procedure TRetDistLeitura.SetleituraResumo(
  const Value: TLeituraResumoCollection);
begin
  FleituraResumo := Value;
end;

function TRetDistLeitura.LerXml: boolean;
var
  Ok: boolean;
  i, j: Integer;
  StrAux: AnsiString;
begin
  Result := False;

  try
    FXML := Self.Leitor.Arquivo;

    if (Leitor.rExtrai(1, 'retOneDistLeitura') <> '') then
    begin
      Fversao    := Leitor.rAtributo('versao', 'retOneDistLeitura');
      FtpAmb     := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic  := Leitor.rCampo(tcStr, 'verAplic');
      FcStat     := Leitor.rCampo(tcInt, 'cStat');
      FxMotivo   := Leitor.rCampo(tcStr, 'xMotivo');
      FdhResp    := Leitor.rCampo(tcDatHor, 'dhResp');
      FultNSU    := Leitor.rCampo(tcStr, 'ultNSU');
      FultNSUONE := Leitor.rCampo(tcStr, 'ultNSUONE');

      i := 0;
      while Leitor.rExtrai(2, 'leitura', '', i + 1) <> '' do
      begin
        FLeitura.New;

        with FLeitura.Items[i] do
        begin
          FNSU   := Leitor.rAtributo('NSU', 'leitura');
          schema := StrToSchemaDFe(Leitor.rAtributo('schema', 'leitura'));
          XML    := RetornarConteudoEntre(Leitor.Grupo, '>', '</leitura');

          if (Leitor.rExtrai(3, 'oneRecepLeitura')) <> '' then
          begin
            with RecepcaoLeitura do
            begin
              Versao   := Leitor.rAtributo('versao', 'oneRecepLeitura');
              tpAmb    := StrToTpAmb(Ok, Leitor.rCampo(tcStr, 'tpAmb'));
              verAplic := Leitor.rCampo(tcStr, 'verAplic');
              tpTransm := StrtotpTransm(Leitor.rCampo(tcStr, 'tpTransm'));
              dhTransm := Leitor.rCampo(tcDatHor, 'dhTransm');

              if (Leitor.rExtrai(3, 'infLeitura')) <> '' then
              begin
                with infLeitura do
                begin
                  cUF             := Leitor.rCampo(tcInt, 'cUF');
                  dhPass          := Leitor.rCampo(tcDatHor, 'dhPass');
                  CNPJOper        := Leitor.rCampo(tcStr, 'CNPJOper');
                  cEQP            := Leitor.rCampo(tcStr, 'cEQP');
                  latitude        := Leitor.rCampo(tcDe6, 'latitude');
                  longitude       := Leitor.rCampo(tcDe6, 'longitude');
                  tpSentido       := StrTotpSentido(Leitor.rCampo(tcStr, 'tpSentido'));
                  placa           := Leitor.rCampo(tcStr, 'placa');
                  tpVeiculo       := StrTotpVeiculo(Leitor.rCampo(tcStr, 'tpVeiculo'));
                  velocidade      := Leitor.rCampo(tcInt, 'velocidade');
                  foto            := Leitor.rCampo(tcStr, 'foto');
                  indiceConfianca := Leitor.rCampo(tcInt, 'indiceConfianca');
                  pesoBrutoTotal  := Leitor.rCampo(tcInt, 'pesoBrutoTotal');
                  nroEixos        := Leitor.rCampo(tcInt, 'nroEixos');
                end;
              end;
            end;
          end;

          j := 0;
          while Leitor.rExtrai(3, 'infMDFe', '', j + 1) <> '' do
          begin
            FinfMDFe.New;
            FinfMDFe.Items[j].chMDFe := Leitor.rCampo(tcStr, 'chMDFe');

            Inc(j);
          end;

          if (Leitor.rExtrai(3, 'infCompl')) <> '' then
          begin
            with infCompl do
            begin
              tpLeitura  := StrTotpLeitura(Leitor.rCampo(tcStr, 'tpLeitura'));
              xEQP       := Leitor.rCampo(tcStr, 'xEQP');
              latitude   := Leitor.rCampo(tcDe6, 'latitude');
              longitude  := Leitor.rCampo(tcDe6, 'longitude');
              placa      := Leitor.rCampo(tcStr, 'placa');
              tpSentido  := StrTotpSentido(Leitor.rCampo(tcStr, 'tpSentido'));
              NSULeitura := Leitor.rCampo(tcStr, 'NSULeitura');
            end;
          end;
        end;

        inc(i);
      end;

      i := 0;
      while Leitor.rExtrai(2, 'leituraResumo', '', i + 1) <> '' do
      begin
        FleituraResumo.New;

        with FleituraResumo.Items[i] do
        begin
          FNSU   := Leitor.rAtributo('NSU', 'leituraResumo');
          schema := StrToSchemaDFe(Leitor.rAtributo('schema', 'leituraResumo'));
          XML    := RetornarConteudoEntre(Leitor.Grupo, '>', '</leituraResumo');

          if (Leitor.rExtrai(3, 'infLeitura')) <> '' then
          begin
            with infLeitura do
            begin
              tpTransm       := StrtotpTransm(Leitor.rCampo(tcStr, 'tpTransm'));
              dhTransm       := Leitor.rCampo(tcDatHor, 'dhTransm');
              cUF            := Leitor.rCampo(tcInt, 'cUF');
              dhPass         := Leitor.rCampo(tcDatHor, 'dhPass');
              CNPJOper       := Leitor.rCampo(tcStr, 'CNPJOper');
              xOper          := Leitor.rCampo(tcStr, 'xOper');
              tpLeitura      := StrTotpLeitura(Leitor.rCampo(tcStr, 'tpLeitura'));
              cEQP           := Leitor.rCampo(tcStr, 'cEQP');
              xEQP           := Leitor.rCampo(tcStr, 'xEQP');
              latitude       := Leitor.rCampo(tcDe6, 'latitude');
              longitude      := Leitor.rCampo(tcDe6, 'longitude');
              tpSentido      := StrTotpSentido(Leitor.rCampo(tcStr, 'tpSentido'));
              placa          := Leitor.rCampo(tcStr, 'placa');
              tpVeiculo      := StrTotpVeiculo(Leitor.rCampo(tcStr, 'tpVeiculo'));
              velocidade     := Leitor.rCampo(tcInt, 'velocidade');
              pesoBrutoTotal := Leitor.rCampo(tcInt, 'pesoBrutoTotal');
              nroEixos       := Leitor.rCampo(tcInt, 'nroEixos');
              NSULeitura     := Leitor.rCampo(tcStr, 'NSULeitura');
            end;
          end;

          j := 0;
          while Leitor.rExtrai(3, 'infMDFe', '', j + 1) <> '' do
          begin
            FinfMDFe.New;
            FinfMDFe.Items[j].chMDFe := Leitor.rCampo(tcStr, 'chMDFe');

            Inc(j);
          end;
        end;

        inc(i);
      end;

      i := 0;
      while Leitor.rExtrai(2, 'leituraCompacta', '', i + 1) <> '' do
      begin
        FleituraCompacta.New;

        with FleituraCompacta.Items[i] do
        begin
          FNSU   := Leitor.rAtributo('NSU', 'leituraCompacta');
          schema := StrToSchemaDFe(Leitor.rAtributo('schema', 'leituraCompacta'));
          XML    := RetornarConteudoEntre(Leitor.Grupo, '>', '</leituraCompacta');

          StrAux      := Leitor.rCampo(tcStr, 'leituraComp');
          leituraComp := UnZip(DecodeBase64(StrAux));

          j := 0;
          while Leitor.rExtrai(3, 'infMDFe', '', j + 1) <> '' do
          begin
            FinfMDFe.New;
            FinfMDFe.Items[j].chMDFe := Leitor.rCampo(tcStr, 'chMDFe');

            Inc(j);
          end;
        end;

        Inc(i);
      end;

      Result := True;
    end;
  except
    on e : Exception do
    begin
//      result := False;
      Raise Exception.Create(e.Message);
    end;
  end;
end;

function TRetDistLeitura.LerXMLFromFile(Const CaminhoArquivo: String): Boolean;
var
  ArqDist: TStringList;
begin
  ArqDist := TStringList.Create;

  try
    ArqDist.LoadFromFile(CaminhoArquivo);

    Self.Leitor.Arquivo := ArqDist.Text;

    Result := LerXml;
  finally
    ArqDist.Free;
  end;
end;

{ TLeituraCompactaCollectionItem }

constructor TLeituraCompactaCollectionItem.Create;
begin
  inherited Create;

  FinfMDFe := TinfMDFeCollection.Create();
end;

destructor TLeituraCompactaCollectionItem.Destroy;
begin
  FinfMDFe.Free;

  inherited;
end;

procedure TLeituraCompactaCollectionItem.SetinfMDFe(
  const Value: TinfMDFeCollection);
begin
  FinfMDFe := Value;
end;

{ TLeituraCompactaCollection }

function TLeituraCompactaCollection.GetItem(
  Index: Integer): TLeituraCompactaCollectionItem;
begin
  Result := TLeituraCompactaCollectionItem(inherited Items[Index]);
end;

function TLeituraCompactaCollection.New: TLeituraCompactaCollectionItem;
begin
  Result := TLeituraCompactaCollectionItem.Create;
  Add(Result);
end;

procedure TLeituraCompactaCollection.SetItem(Index: Integer;
  Value: TLeituraCompactaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfMDFeCollection }

function TinfMDFeCollection.GetItem(Index: Integer): TinfMDFeCollectionItem;
begin
  Result := TinfMDFeCollectionItem(inherited Items[Index]);
end;

function TinfMDFeCollection.New: TinfMDFeCollectionItem;
begin
  Result := TinfMDFeCollectionItem.Create;
  Add(Result);
end;

procedure TinfMDFeCollection.SetItem(Index: Integer;
  Value: TinfMDFeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TLeituraResumoCollectionItem }

constructor TLeituraResumoCollectionItem.Create;
begin
  inherited Create;

  FinfLeitura := TinfLeitura.Create;
  FinfMDFe := TinfMDFeCollection.Create();
end;

destructor TLeituraResumoCollectionItem.Destroy;
begin
  FinfLeitura.Free;
  FinfMDFe.Free;

  inherited;
end;

procedure TLeituraResumoCollectionItem.SetinfMDFe(
  const Value: TinfMDFeCollection);
begin
  FinfMDFe := Value;
end;

{ TLeituraResumoCollection }

function TLeituraResumoCollection.GetItem(
  Index: Integer): TLeituraResumoCollectionItem;
begin
  Result := TLeituraResumoCollectionItem(inherited Items[Index]);
end;

function TLeituraResumoCollection.New: TLeituraResumoCollectionItem;
begin
  Result := TLeituraResumoCollectionItem.Create;
  Add(Result);
end;

procedure TLeituraResumoCollection.SetItem(Index: Integer;
  Value: TLeituraResumoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.

