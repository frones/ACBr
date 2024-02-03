unit pcnReinfR2055;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnGerador,
  ACBrUtil.Base, ACBrUtil.FilesIO,
  ACBrDFeConsts,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  TR2055Collection = class;
  TR2055CollectionItem = class;
  TevtAqProd = class;

  {Classes específicas deste evento}
  TinfoAquisProd = class;
  TideEstab = class;
  TtipoComCollection = class;
  TtipoComCollectionItem = class;
  TinfoProcCollection = class;
  TinfoProcCollectionItem = class;

  TR2055Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR2055CollectionItem;
    procedure SetItem(Index: Integer; Value: TR2055CollectionItem);
  public
    function Add: TR2055CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR2055CollectionItem;

    property Items[Index: Integer]: TR2055CollectionItem read GetItem write SetItem; default;
  end;

  TR2055CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtAqProd: TevtAqProd;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtAqProd: TevtAqProd read FevtAqProd write FevtAqProd;
  end;

  TevtAqProd = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FinfoAquisProd: TinfoAquisProd;

    {Geradores específicos desta classe}
    procedure GerarideEstab;
    procedure GerarideProdutor;
    procedure GerarAquis(Lista: TtipoComCollection);
    procedure GerarinfoProc(Lista: TinfoProcCollection);
  public
    constructor Create(AACBrReinf: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property infoAquisProd: TinfoAquisProd read FinfoAquisProd write FinfoAquisProd;
  end;

  { TinfoComProd }
  TinfoAquisProd = class(TObject)
  private
    FideEstabAdquir: TideEstab;
  public
    constructor Create;
    destructor Destroy; override;
    property ideEstabAdquir: TideEstab read FideEstabAdquir write FideEstabAdquir;
  end;

  { TideEstab }
  TideEstab = class(TObject)
  private
    FtpInscAdq: TtpInsc;
    FnrInscAdq: string;
    FindOpcCP: string;
    FnrInscProd: string;
    FtpInscProd: TtpInsc;
    FdetAquis: TtipoComCollection;
    FideProdutor: TtipoComCollection;
 public
    constructor Create;
    destructor Destroy; override;

    property tpInscAdq: TtpInsc read FtpInscAdq write FtpInscAdq default tiCNPJ;
    property nrInscAdq: string read FnrInscAdq write FnrInscAdq;
    property indOpcCP: string read FindOpcCP write FindOpcCP;
    property nrInscProd: string read FnrInscProd write FnrInscProd;
    property tpInscProd: TtpInsc read FtpInscProd write FtpInscProd default tiCNPJ;
    property detAquis: TtipoComCollection read FdetAquis write FdetAquis;
    property ideProdutor: TtipoComCollection read FideProdutor write FideProdutor;
  end;

  TtipoComCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TtipoComCollectionItem;
    procedure SetItem(Index: Integer; Value: TtipoComCollectionItem);
  public
    function Add: TtipoComCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TtipoComCollectionItem;

    property Items[Index: Integer]: TtipoComCollectionItem read GetItem write SetItem; default;
  end;

  TtipoComCollectionItem = class(TObject)
  private
    FindAquis: TdetAquis;
    FvlrBruto: double;
    FvlrCPDescPR: double;
    FvlrRatDescPR: double;
    FvlrSenarDesc: double;
    FinfoProc: TinfoProcCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property indAquis: TdetAquis read FindAquis write FindAquis;
    property vlrBruto: double read FvlrBruto write FvlrBruto;
    property vlrCPDescPR: double read FvlrCPDescPR write FvlrCPDescPR;
    property vlrRatDescPR: double read FvlrRatDescPR write FvlrRatDescPR;
    property vlrSenarDesc: double read FvlrSenarDesc write FvlrSenarDesc;
    property infoProc: TinfoProcCollection read FinfoProc write FinfoProc;
  end;

  TinfoProcCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoProcCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoProcCollectionItem);
  public
    function Add: TinfoProcCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoProcCollectionItem;

    property Items[Index: Integer]: TinfoProcCollectionItem read GetItem write SetItem; default;
  end;

  TinfoProcCollectionItem = class(TObject)
  private
    FtpProc: TtpProc;
    FnrProc: String;
    FcodSusp: String;
    FvlrCPNRet: double;
    FvlrRatNRet: double;
    FvlrSenarNRet: double;
  public
    property tpProc: TtpProc read FtpProc write FtpProc;
    property nrProc: String read FnrProc write FnrProc;
    property codSusp: String read FcodSusp write FcodSusp;
    property vlrCPNRet: double read FvlrCPNRet write FvlrCPNRet;
    property vlrRatNRet: double read FvlrRatNRet write FvlrRatNRet;
    property vlrSenarNRet: double read FvlrSenarNRet write FvlrSenarNRet;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR2055Collection }

function TR2055Collection.Add: TR2055CollectionItem;
begin
  Result := Self.New;
end;

function TR2055Collection.GetItem(Index: Integer): TR2055CollectionItem;
begin
  Result := TR2055CollectionItem(inherited Items[Index]);
end;

function TR2055Collection.New: TR2055CollectionItem;
begin
  Result := TR2055CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR2055Collection.SetItem(Index: Integer; Value: TR2055CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR2055CollectionItem }

constructor TR2055CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento := teR2055;
  FevtAqProd := TevtAqProd.Create(AOwner);
end;

destructor TR2055CollectionItem.Destroy;
begin
  inherited;

  FevtAqProd.Free;
end;

{ TevtComProd }

constructor TevtAqProd.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri := TideContri.Create;
  FIdeEvento := TIdeEvento2.Create;
  FinfoAquisProd := TinfoAquisProd.Create;
end;

destructor TevtAqProd.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FinfoAquisProd.Free;

  inherited;
end;

{ TinfoComProd }

constructor TinfoAquisProd.Create;
begin
  FideEstabAdquir := TideEstab.Create;
end;

destructor TinfoAquisProd.Destroy;
begin
  FideEstabAdquir.Free;

  inherited;
end;

{ TideEstab }

constructor TideEstab.Create;
begin
  FdetAquis := TtipoComCollection.Create;
  FideProdutor := TtipoComCollection.Create;
end;

destructor TideEstab.Destroy;
begin
  FdetAquis.Free;
  FideProdutor.Free;

  inherited;
end;

{ TtipoComCollection }

function TtipoComCollection.Add: TtipoComCollectionItem;
begin
  Result := Self.New;
end;

function TtipoComCollection.GetItem(
  Index: Integer): TtipoComCollectionItem;
begin
  Result := TtipoComCollectionItem(inherited Items[Index]);
end;

function TtipoComCollection.New: TtipoComCollectionItem;
begin
  Result := TtipoComCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtipoComCollection.SetItem(Index: Integer;
  Value: TtipoComCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TtipoComCollectionItem }

constructor TtipoComCollectionItem.Create;
begin
  FinfoProc := TinfoProcCollection.Create;
end;

destructor TtipoComCollectionItem.Destroy;
begin
  FinfoProc.Free;

  inherited;
end;

{ TinfoProcCollection }

function TinfoProcCollection.Add: TinfoProcCollectionItem;
begin
  Result := Self.New;
end;

function TinfoProcCollection.GetItem(
  Index: Integer): TinfoProcCollectionItem;
begin
  Result := TinfoProcCollectionItem(inherited Items[Index]);
end;

function TinfoProcCollection.New: TinfoProcCollectionItem;
begin
  Result := TinfoProcCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoProcCollection.SetItem(Index: Integer;
  Value: TinfoProcCollectionItem);
begin
  inherited Items[Index] := Value;
end;

procedure TevtAqProd.GerarideEstab;
begin
  Gerador.wGrupo('ideEstabAdquir');

  Gerador.wCampo(tcStr, '', 'tpInscAdq', 1,  1, 1, TpInscricaoToStr(Self.FinfoAquisProd.ideEstabAdquir.tpInscAdq));
  Gerador.wCampo(tcStr, '', 'nrInscAdq', 1, 14, 1, Self.FinfoAquisProd.ideEstabAdquir.nrInscAdq);

  GerarideProdutor;

  Gerador.wGrupo('/ideEstabAdquir');
end;

procedure TevtAqProd.GerarideProdutor;
begin
  Gerador.wGrupo('ideProdutor');

  Gerador.wCampo(tcStr, '', 'tpInscProd', 1,  1, 1, TpInscricaoToStr(Self.FinfoAquisProd.ideEstabAdquir.tpInscProd));
  Gerador.wCampo(tcStr, '', 'nrInscProd', 1, 14, 1, Self.FinfoAquisProd.ideEstabAdquir.nrInscProd);
  if Self.FinfoAquisProd.ideEstabAdquir.indOpcCP<>'' then
    Gerador.wCampo(tcStr, '', 'indOpcCP',   1,  1, 1, Self.FinfoAquisProd.ideEstabAdquir.indOpcCP);

  GerarAquis(Self.FinfoAquisProd.ideEstabAdquir.detAquis);

  Gerador.wGrupo('/ideProdutor');
end;

procedure TevtAqProd.GerarAquis(Lista: TtipoComCollection);
var
  item: TtipoComCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('detAquis');

    Gerador.wCampo(tcStr, '', 'indAquis',     1,  1, 1, detAquisToStr( item.indAquis));
    Gerador.wCampo(tcDe2, '', 'vlrBruto',     1, 14, 1, item.vlrBruto);
    Gerador.wCampo(tcDe2, '', 'vlrCPDescPR',  1, 14, 1, item.vlrCPDescPR);
    Gerador.wCampo(tcDe2, '', 'vlrRatDescPR', 1, 14, 1, item.vlrRatDescPR);
    Gerador.wCampo(tcDe2, '', 'vlrSenarDesc', 1, 14, 1, item.vlrSenarDesc);

    GerarinfoProc(item.infoProc);

    Gerador.wGrupo('/detAquis');
  end;

  if Lista.Count > 4 then
    Gerador.wAlerta('', 'detAquis', 'Lista de Tipos de Comercialização', ERR_MSG_MAIOR_MAXIMO + '4');
end;

procedure TevtAqProd.GerarinfoProc(Lista: TinfoProcCollection);
var
  item: TinfoProcCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoProcJud');
    Gerador.wCampo(tcStr, '', 'nrProcJud',    1, 21, 1, item.nrProc);
    Gerador.wCampo(tcStr, '', 'codSusp',      1, 14, 0, item.codSusp);
    Gerador.wCampo(tcDe2, '', 'vlrCPNRet',    1, 14, 0, item.vlrCPNRet);
    Gerador.wCampo(tcDe2, '', 'vlrRatNRet',   1, 14, 0, item.vlrRatNRet);
    Gerador.wCampo(tcDe2, '', 'vlrSenarNRet', 1, 14, 0, item.vlrSenarNRet);

    Gerador.wGrupo('/infoProcJud');
  end;

  if Lista.Count > 50 then
    Gerador.wAlerta('', 'infoProcJud', 'Lista de Informações de Processos', ERR_MSG_MAIOR_MAXIMO + '50');
end;

function TevtAqProd.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evt2055AquisicaoProdRural');
    Gerador.wGrupo('evtAqProd id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    Gerador.wGrupo('infoAquisProd');

    GerarideEstab;

    Gerador.wGrupo('/infoAquisProd');
    Gerador.wGrupo('/evtAqProd');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtAqProd.LerArqIni(const AIniString: String): Boolean;
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

    // ********************************************
    // Precisa checar se não esta faltando nada
    // ********************************************

    with Self do
    begin
      sSecao := 'evtAqProd';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif := StrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.perApur  := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi  := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      { Excluído na Versão 2.01
      Indicativo de retificação de informação enviada ao ambiente nacional do eSocial.
      Validação: Informação permitida apenas se perApur for anterior ao início
      de vigência do evento R-2055 na EFD-Reinf.
      Só pode ser informado se indRetif = [1].
      Valores válidos: S.
      }
      ideEvento.retifS1250  := INIRec.ReadString(sSecao, 'retifS1250', EmptyStr);

      sSecao := 'ideContri';

      ideContri.OrgaoPublico := (TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte = tcOrgaoPublico);
      ideContri.TpInsc       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
	  
      sSecao := 'ideEstabAdq';

      FinfoAquisProd.ideEstabAdquir.tpInscAdq := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscAdq', '1'));
      FinfoAquisProd.ideEstabAdquir.nrInscAdq := INIRec.ReadString(sSecao, 'nrInscAdq', EmptyStr);
	  
      FinfoAquisProd.ideEstabAdquir.tpInscProd := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscProd', '1'));
      FinfoAquisProd.ideEstabAdquir.nrInscProd := INIRec.ReadString(sSecao, 'nrInscProd', EmptyStr);
      FinfoAquisProd.ideEstabAdquir.indOpcCP   := INIRec.ReadString(sSecao, 'indOpcCP', EmptyStr);


      with FinfoAquisProd.ideEstabAdquir do
      begin
        I := 1;
        while true do
        begin
          // de 1 até 3
          sSecao := 'tipoCom' + IntToStrZero(I, 1);
          sFim   := INIRec.ReadString(sSecao, 'indCom', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with detAquis.New do
          begin
            indAquis := StrToDetAquis(Ok, sFim);
            vlrBruto := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBruto', ''), 0);
            vlrCPDescPR  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCPDescPR', ''), 0);
            vlrRatDescPR := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRatDescPR', ''), 0);
            vlrSenarDesc := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrSenarDesc', ''), 0);

            J := 1;
            while true do
            begin
              // de 01 até 50
              sSecao := 'infoProcJud' + IntToStrZero(I, 1) + IntToStrZero(J, 2);
              sFim   := INIRec.ReadString(sSecao, 'tpProc', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;
            
              with infoProc.New do
              begin
                tpProc       := StrToTpProc(Ok, sFim);
                nrProc       := INIRec.ReadString(sSecao, 'nrProc', '');
                codSusp      := INIRec.ReadString(sSecao, 'codSusp', '');
                vlrCPNRet    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCPNRet', ''), 0);
                vlrRatNRet   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRatNRet', ''), 0);
                vlrSenarDesc := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrSenarDesc', ''), 0);
              end;
            
              Inc(J);
            end;
            
          end;

          Inc(I);
        end;
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
