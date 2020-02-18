{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
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

unit pcesS1207;

interface

uses
  SysUtils, Classes, Dialogs, Controls,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type

  TEvtBenPrRP = class;
  TS1207CollectionItem = class;
  TS1207Collection = class;
  TDMDevCollection = class;
  TDMDevCollectionItem = class;
  TIdeBenef = class;
  TItensCollection = class;
  TItensCollectionItem = class;

  TS1207Collection = class(TeSocialCollection)
  private
    function GetItem(Index: integer): TS1207CollectionItem;
    procedure SetItem(Index: integer; Value: TS1207CollectionItem);
  public
    function Add: TS1207CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1207CollectionItem;
    property Items[Index: integer]: TS1207CollectionItem read GetItem write SetItem; default;
  end;

  TS1207CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtBenPrRP: TEvtBenPrRP;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtBenPrRP: TEvtBenPrRP read FEvtBenPrRP write FEvtBenPrRP;
  end;

  TDMDevCollection = class(TACBrObjectList)
  private
    function GetItem(Index: integer): TDMDevCollectionItem;
    procedure SetItem(Index: integer; Value: TDMDevCollectionItem);
  public
    function Add: TDMDevCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDMDevCollectionItem;
    property Items[Index: integer]: TDMDevCollectionItem read GetItem write SetItem; default;
  end;

  TDMDevCollectionItem = class(TObject)
  private
    FTpBenef: Integer;
    FNrBenefic: string;
    FIdeDmDev: string;
    FItens: TItensCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property tpBenef: integer read FTpBenef write FTpBenef;
    property nrBenefic: string read FNrBenefic write FNrBenefic;
    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property itens: TItensCollection read FItens write FItens;
  end;

  TItensCollection = class(TACBrObjectList)
  private
    function GetItem(Index: integer): TItensCollectionItem;
    procedure SetItem(Index: integer; Value: TItensCollectionItem);
  public
    function Add: TItensCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TItensCollectionItem;
    property Items[Index: integer]: TItensCollectionItem read GetItem write SetItem; default;
  end;

  TItensCollectionItem = class(TObject)
  private
    FCodRubr: string;
    FIdeTabRubr: string;
    FVrRubr: double;
  public
    property codRubr: string read FCodRubr write FCodRubr;
    property ideTabRubr: string read FIdeTabRubr write FIdeTabRubr;
    property vrRubr: double read FVrRubr write FVrRubr;
  end;

  TEvtBenPrRP = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FIdeBenef: TIdeBenef;
    FDMDev: TDMDevCollection;

    {Geradores específicos desta classe}
    procedure GerarIdeBenef;
    procedure GerarDmDev;
    procedure GerarItens(pItens: TItensCollection);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ideBenef: TIdeBenef read FIdeBenef write FIdeBenef;
    property dmDev: TDMDevCollection read FDMDev write FDMDev;
  end;

  TIdeBenef = class(TObject)
  private
    FCpfBenef: string;
  public
    property cpfBenef: string read FCpfBenef write FCpfBenef;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TItensCollection }

function TItensCollection.Add: TItensCollectionItem;
begin
  Result := Self.New;
end;

function TItensCollection.GetItem(Index: integer): TItensCollectionItem;
begin
  Result := TItensCollectionItem(inherited Items[Index]);
end;

procedure TItensCollection.SetItem(Index: integer; Value: TItensCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TItensCollection.New: TItensCollectionItem;
begin
  Result := TItensCollectionItem.Create;
  Self.Add(Result);
end;

{ TDMDevCollection }

function TDMDevCollection.Add: TDMDevCollectionItem;
begin
  Result := Self.New;
end;

function TDMDevCollection.GetItem(Index: integer): TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited Items[Index]);
end;

procedure TDMDevCollection.SetItem(Index: integer; Value: TDMDevCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDMDevCollection.New: TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem.Create;
  Self.Add(Result);
end;

{ TDMDevCollectionItem }

constructor TDMDevCollectionItem.Create;
begin
  inherited Create;
  FItens := TItensCollection.Create;
end;

destructor TDMDevCollectionItem.Destroy;
begin
  FItens.Free;

  inherited;
end;

{ TEvtBenPrRP }
constructor TEvtBenPrRP.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeBenef      := TIdeBenef.Create;
  FDMDev         := TDMDevCollection.Create;
end;

destructor TEvtBenPrRP.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeBenef.Free;
  FDMDev.Free;

  inherited;
end;

procedure TEvtBenPrRP.GerarIdeBenef;
begin
  Gerador.wGrupo('ideBenef');

  Gerador.wCampo(tcStr, '', 'cpfBenef', 11, 11, 1, ideBenef.cpfBenef);

  Gerador.wGrupo('/ideBenef');
end;

procedure TEvtBenPrRP.GerarItens(pItens: TItensCollection);
var
  i: integer;
begin
  for i := 0 to pItens.Count - 1 do
  begin
    Gerador.wGrupo('itens');

    Gerador.wCampo(tcStr, '', 'codRubr',    1, 30, 1, pItens[i].codRubr);
    Gerador.wCampo(tcStr, '', 'ideTabRubr', 1,  8, 1, pItens[i].ideTabRubr);
    Gerador.wCampo(tcDe2, '', 'vrRubr',     1, 14, 1, pItens[i].vrRubr);

    Gerador.wGrupo('/itens');
  end;

  if pItens.Count > 99 then
    Gerador.wAlerta('', 'itens', 'Lista de Detalhamento de Valores', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtBenPrRP.GerarDmDev;
var
  i: integer;
begin
  for i := 0 to dmDev.Count - 1 do
  begin
    Gerador.wGrupo('dmDev');

    Gerador.wCampo(tcInt, '', 'tpBenef',   2,   2, 1, dmDev[i].tpBenef);
    Gerador.wCampo(tcStr, '', 'nrBenefic', 1, 200, 1, dmDev[i].nrBenefic);
    Gerador.wCampo(tcStr, '', 'ideDmDev',  1,  30, 1, dmDev[i].ideDmDev);

    GerarItens(dmDev[i].itens);

    Gerador.wGrupo('/dmDev');
  end;

  if dmDev.Count > 99 then
    Gerador.wAlerta('', 'dmDev', 'Lista de Demostrativos', ERR_MSG_MAIOR_MAXIMO + '99');
end;

function TEvtBenPrRP.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtBenPrRP');
    Gerador.wGrupo('evtBenPrRP Id="' + Self.Id + '"');

    GerarIdeEvento3(Self.IdeEvento);
    GerarIdeEmpregador(Self.ideEmpregador);
    GerarIdeBenef;
    GerarDmDev;

    Gerador.wGrupo('/evtBenPrRP');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtBenPrRP');

//    Validar(schevtBenPrRP);
  except
    on e: Exception do
      raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TEvtBenPrRP.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtBenPrRP';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.IndApuracao := eSStrToIndApuracao(Ok, INIRec.ReadString(sSecao, 'indApuracao', '1'));
      ideEvento.perApur     := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideBenef';
      ideBenef.cpfBenef := INIRec.ReadString(sSecao, 'cpfBenef', EmptyStr);

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'dmDev' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'nrBenefic', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with dmDev.New do
        begin
          tpBenef   := INIRec.ReadInteger(sSecao, 'tpBenef', 0);
          nrBenefic := sFim;
          ideDmDev  := INIRec.ReadString(sSecao, 'ideDmDev', '');

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'itens' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with itens.New do
            begin
              codRubr    := sFim;
              ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', '');
              vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
            end;

            Inc(J);
          end;

        end;

        Inc(I);
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

{ TS1207CollectionItem }
constructor TS1207CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS1207;
  FEvtBenPrRP := TEvtBenPrRP.Create(AOwner);
end;

destructor TS1207CollectionItem.Destroy;
begin
  FEvtBenPrRP.Free;

  inherited;
end;

{ TS1207Collection }
function TS1207Collection.Add: TS1207CollectionItem;
begin
  Result := Self.New;
end;

function TS1207Collection.GetItem(Index: integer): TS1207CollectionItem;
begin
  Result := TS1207CollectionItem(inherited Items[Index]);
end;

procedure TS1207Collection.SetItem(Index: integer; Value: TS1207CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1207Collection.New: TS1207CollectionItem;
begin
  Result := TS1207CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

end.
