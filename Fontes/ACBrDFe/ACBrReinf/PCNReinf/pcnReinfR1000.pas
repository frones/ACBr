{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 04/12/2017: Renato Rubinho
|*  - Implementados registros que faltavam e isoladas as respectivas classes
*******************************************************************************}

{$I ACBr.inc}

unit pcnReinfR1000;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, ACBrUtil,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  TR1000Collection = class;
  TR1000CollectionItem = class;
  TevtInfoContri = class;

  {Classes específicas deste evento}
  TinfoContribuinte = class;
  TInfoCadastro = class;
  TContato = class;
  TSoftwareHouseCollection = class;
  TSoftwareHouseCollectionItem = class;
  TinfoEFR = class;

  TR1000Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TR1000CollectionItem;
    procedure SetItem(Index: Integer; Value: TR1000CollectionItem);
  public
    function Add: TR1000CollectionItem;
    property Items[Index: Integer]: TR1000CollectionItem read GetItem write SetItem; default;
  end;

  TR1000CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FevtInfoContri: TevtInfoContri;
    procedure setevtInfoContri(const Value: TevtInfoContri);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtInfoContri: TevtInfoContri read FevtInfoContri write setevtInfoContri;
  end;

  TevtInfoContri = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FModoLancamento: TTipoOperacao;
    FIdeEvento: TIdeEvento;
    FideContri: TideContri;
    FinfoContribuinte: TinfoContribuinte;
    FACBrReinf: TObject;

    {Geradores específicos desta classe}
    procedure GerarInfoCadastro;
    procedure GerarContato;
    procedure GerarSoftwareHouse;
    procedure GerarInfoEFR;
  public
    constructor Create(AACBrReinf: TObject); overload;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TTipoOperacao read FModoLancamento write FModoLancamento;
    property ideEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property infoContribuinte: TinfoContribuinte read FinfoContribuinte write FinfoContribuinte;
  end;

  TinfoContribuinte = class(TPersistent)
  private
    FidePeriodo: TIdePeriodo;
    FinfoCadastro: TInfoCadastro;
    FNovaValidade: TidePeriodo;

    function getInfoCadastro(): TInfoCadastro;
    function getNovaValidade(): TidePeriodo;
  public
    constructor Create;
    destructor Destroy; override;

    function infoCadastroInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property idePeriodo: TIdePeriodo read FidePeriodo write FidePeriodo;
    property infoCadastro: TInfoCadastro read getInfoCadastro write FinfoCadastro;
    property novaValidade: TIdePeriodo read getNovaValidade write FnovaValidade;
  end;

  TInfoCadastro = class
   private
    FClassTrib: TpClassTrib;
    FindEscrituracao: TindEscrituracao;
    FindDesoneracao: TindDesoneracao;
    FindAcordoIsenMulta: TindAcordoIsenMulta;
    FindSitPJ: TindSitPJ;
    FContato: TContato;
    FSoftwareHouse: TSoftwareHouseCollection;
    FinfoEFR: TinfoEFR;
  public
    constructor Create;
    destructor Destroy; override;

    property ClassTrib: TpClassTrib read FClassTrib write FClassTrib;
    property indEscrituracao: TindEscrituracao read FindEscrituracao write FindEscrituracao default ieNaoObrig;
    property indDesoneracao: TindDesoneracao read FindDesoneracao write FindDesoneracao default idNaoAplic;
    property indAcordoIsenMulta: TindAcordoIsenMulta read FindAcordoIsenMulta write FindAcordoIsenMulta default aiSemAcordo;
    property indSitPJ: TindSitPJ read FindSitPJ write FindSitPJ default spNormal;
    property Contato: TContato read FContato write FContato;
    property SoftwareHouse: TSoftwareHouseCollection read FSoftwareHouse write FSoftwareHouse;
    property infoEFR: TinfoEFR read FinfoEFR write FinfoEFR;
  end;

  TContato = class(TPersistent)
  private
    FNmCtt: string;
    FCpfCtt: string;
    FFoneFixo: string;
    FFoneCel: string;
    FEmail: string;
  public
    property NmCtt: string read FNmCtt write FNmCtt;
    property CpfCtt: string read FCpfCtt write FCpfCtt;
    property FoneFixo: string read FFoneFixo write FFoneFixo;
    property FoneCel: string read FFoneCel write FFoneCel;
    property Email: string read FEmail write FEmail;
  end;

  TSoftwareHouseCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TSoftwareHouseCollectionItem;
    procedure SetItem(Index: Integer; Value: TSoftwareHouseCollectionItem);
  public
    constructor create(); reintroduce;
    function Add: TSoftwareHouseCollectionItem;
    property Items[Index: Integer]: TSoftwareHouseCollectionItem read GetItem write SetItem; default;
  end;

  TSoftwareHouseCollectionItem = class(TCollectionItem)
  private
    FCnpjSoftHouse: String;
    FNmRazao: String;
    FNmCont: String;
    FTelefone: String;
    Femail: String;
  public
    property CnpjSoftHouse: String read FCnpjSoftHouse write FCnpjSoftHouse;
    property NmRazao: String read FNmRazao write FNmRazao;
    property NmCont: String read FNmCont write FNmCont;
    property Telefone: String read FTelefone write FTelefone;
    property email: String read Femail write Femail;
  end;

  TInfoEFR = class(TPersistent)
  private
     FideEFR: TtpSimNao;
     FcnpjEFR: String;
  public
    property ideEFR: TtpSimNao read FideEFR write FideEFR;
    property cnpjEFR: String read FcnpjEFR write FcnpjEFR;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR1000Collection }

function TR1000Collection.Add: TR1000CollectionItem;
begin
  Result := TR1000CollectionItem(inherited Add);
end;

function TR1000Collection.GetItem(Index: Integer): TR1000CollectionItem;
begin
  Result := TR1000CollectionItem(inherited GetItem(Index));
end;

procedure TR1000Collection.SetItem(Index: Integer; Value: TR1000CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TR1000CollectionItem }

procedure TR1000CollectionItem.AfterConstruction;
begin
  inherited;
  FTipoEvento := teR1000;
  FevtInfoContri := TevtInfoContri.Create(Collection.Owner);
end;

procedure TR1000CollectionItem.BeforeDestruction;
begin
  inherited;
  FevtInfoContri.Free;
end;

procedure TR1000CollectionItem.setevtInfoContri(const Value: TevtInfoContri);
begin
  FevtInfoContri.Assign(Value);
end;

{ TinfoContribuinte }

constructor TinfoContribuinte.Create;
begin
  inherited;

  FidePeriodo   := TIdePeriodo.Create;
  FinfoCadastro := nil;
  FNovaValidade := nil;
end;

destructor TinfoContribuinte.Destroy;
begin
  FidePeriodo.Free;
  FreeAndNil(FinfoCadastro);
  FreeAndNil(FNovaValidade);

  inherited;
end;

function TinfoContribuinte.getInfoCadastro: TInfoCadastro;
begin
  if Not(Assigned(FinfoCadastro)) then
    FinfoCadastro := TInfoCadastro.Create;
  Result := FinfoCadastro;
end;

function TinfoContribuinte.getNovaValidade: TidePeriodo;
begin
  if Not(Assigned(FNovaValidade)) then
    FNovaValidade := TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TinfoContribuinte.infoCadastroInst: Boolean;
begin
  Result := Assigned(FinfoCadastro);
end;

function TinfoContribuinte.novaValidadeInst: Boolean;
begin
  Result := Assigned(FNovaValidade);
end;

{ TInfoCadastro }

constructor TInfoCadastro.Create;
begin
  FContato       := TContato.Create;
  FSoftwareHouse := TSoftwareHouseCollection.Create;
  FinfoEFR       := TinfoEFR.Create;
end;

destructor TInfoCadastro.Destroy;
begin
  FContato.Free;
  FSoftwareHouse.Free;
  FinfoEFR.Free;

  inherited;
end;

{ TSoftwareHouseCollection }

function TSoftwareHouseCollection.Add: TSoftwareHouseCollectionItem;
begin
  Result := TSoftwareHouseCollectionItem(inherited add());
//  Result.Create;
end;

constructor TSoftwareHouseCollection.create;
begin
  Inherited create(TSoftwareHouseCollectionItem);
end;

function TSoftwareHouseCollection.GetItem(
  Index: Integer): TSoftwareHouseCollectionItem;
begin
  Result := TSoftwareHouseCollectionItem(inherited GetItem(Index));
end;

procedure TSoftwareHouseCollection.SetItem(Index: Integer;
  Value: TSoftwareHouseCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TevtInfoContri }

constructor TevtInfoContri.Create(AACBrReinf: TObject);
begin
  inherited;

  FACBrReinf := AACBrReinf;

  FideContri        := TideContri.create;
  FIdeEvento        := TIdeEvento.create;
  FinfoContribuinte := TinfoContribuinte.Create;
end;

destructor TevtInfoContri.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FinfoContribuinte.Free;

  inherited;
end;

procedure TevtInfoContri.GerarInfoCadastro;
begin
  Gerador.wGrupo('infoCadastro');

  Gerador.wCampo(tcStr, '', 'classTrib',          2, 002, 1, tpClassTribToStr(Self.infoContribuinte.infoCadastro.ClassTrib));
  Gerador.wCampo(tcStr, '', 'indEscrituracao',    1,   1, 1, indEscrituracaoToStr(Self.infoContribuinte.infoCadastro.indEscrituracao));
  Gerador.wCampo(tcStr, '', 'indDesoneracao',     1,   1, 1, indDesoneracaoToStr(Self.infoContribuinte.infoCadastro.indDesoneracao));
  Gerador.wCampo(tcStr, '', 'indAcordoIsenMulta', 1,   1, 1, indAcordoIsenMultaToStr(Self.infoContribuinte.infoCadastro.indAcordoIsenMulta));
  Gerador.wCampo(tcStr, '', 'indSitPJ',           1,   1, 0, indSitPJToStr(Self.infoContribuinte.infoCadastro.indSitPJ));

  GerarContato;
  GerarSoftwareHouse;
  GerarInfoEFR;

  Gerador.wGrupo('/infoCadastro');
end;

procedure TevtInfoContri.GerarContato;
begin
  Gerador.wGrupo('contato');

  Gerador.wCampo(tcStr, '', 'nmCtt',     1, 70, 1, Self.infoContribuinte.InfoCadastro.Contato.NmCtt);
  Gerador.wCampo(tcStr, '', 'cpfCtt',   11, 11, 1, Self.infoContribuinte.infoCadastro.Contato.CpfCtt);
  Gerador.wCampo(tcStr, '', 'foneFixo',  1, 13, 0, Self.infoContribuinte.infoCadastro.Contato.FoneFixo);
  Gerador.wCampo(tcStr, '', 'foneCel',   1, 13, 0, Self.infoContribuinte.infoCadastro.Contato.FoneCel);
  Gerador.wCampo(tcStr, '', 'email',     1, 60, 0, Self.infoContribuinte.infoCadastro.Contato.email);

  Gerador.wGrupo('/contato');
end;

procedure TevtInfoContri.GerarSoftwareHouse;
var
  i: Integer;
begin
  for i := 0 to infoContribuinte.infoCadastro.SoftwareHouse.Count - 1 do
  begin
    Gerador.wGrupo('softHouse');

    Gerador.wCampo(tcStr, '', 'cnpjSoftHouse', 14,  14, 1, infoContribuinte.infoCadastro.SoftwareHouse[i].CnpjSoftHouse);
    Gerador.wCampo(tcStr, '', 'nmRazao',        1, 100, 1, infoContribuinte.infoCadastro.SoftwareHouse[i].NmRazao);
    Gerador.wCampo(tcStr, '', 'nmCont',         1,  70, 1, infoContribuinte.infoCadastro.SoftwareHouse[i].NmCont);
    Gerador.wCampo(tcStr, '', 'telefone',       1,  13, 0, infoContribuinte.infoCadastro.SoftwareHouse[i].Telefone);
    Gerador.wCampo(tcStr, '', 'email',          1,  60, 0, infoContribuinte.infoCadastro.SoftwareHouse[i].email);

    Gerador.wGrupo('/softHouse');
  end;

  if infoContribuinte.infoCadastro.SoftwareHouse.Count > 99 then
    Gerador.wAlerta('', 'softHouse', 'Lista de Software House', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TevtInfoContri.GerarInfoEFR;
begin
  if TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte in [tcOrgaoPublico] then
  begin
    Gerador.wGrupo('infoEFR');

    Gerador.wCampo(tcStr, '', 'ideEFR',   1,  1, 1, SimNaoToStr(infoContribuinte.infoCadastro.infoEFR.ideEFR));
    Gerador.wCampo(tcStr, '', 'cnpjEFR', 14, 14, 0, infoContribuinte.infoCadastro.infoEFR.cnpjEFR);

    Gerador.wGrupo('/infoEFR')
  end;
end;

function TevtInfoContri.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial);

    GerarCabecalho('evtInfoContribuinte');
    Gerador.wGrupo('evtInfoContri id="' + Self.Id + '"');

    GerarIdeEvento(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    Gerador.wGrupo('infoContri');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdePeriodo(Self.infoContribuinte.idePeriodo);

    if (Self.ModoLancamento <> toExclusao) then
    begin
      GerarInfoCadastro;
      if ModoLancamento = toAlteracao then
        if (infoContribuinte.novaValidadeInst()) then
          GerarIdePeriodo(infoContribuinte.novaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoContri');
    Gerador.wGrupo('/evtInfoContri');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtInfoContri');

    Validar(schevtInfoContribuinte);
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtInfoContri.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I: Integer;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtInfoContri';
      Id             := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial     := INIRec.ReadInteger(sSecao, 'Sequencial', 0);
      ModoLancamento := StrToTipoOperacao(Ok, INIRec.ReadString(sSecao, 'ModoLancamento', 'inclusao'));

      sSecao := 'ideEvento';
      ideEvento.TpAmb   := StrTotpAmbReinf(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
      ideEvento.ProcEmi := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.OrgaoPublico := (TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte = tcOrgaoPublico);
      ideContri.TpInsc       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'idePeriodo';
      infoContribuinte.idePeriodo.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoContribuinte.idePeriodo.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (ModoLancamento <> toExclusao) then
      begin
        sSecao := 'infoCadastro';
        infoContribuinte.infoCadastro.ClassTrib          := StrTotpClassTrib(Ok, INIRec.ReadString(sSecao, 'classTrib', '00'));
        infoContribuinte.infoCadastro.indEscrituracao    := StrToindEscrituracao(Ok, INIRec.ReadString(sSecao, 'indEscrituracao', '0'));
        infoContribuinte.infoCadastro.indDesoneracao     := StrToindDesoneracao(Ok, INIRec.ReadString(sSecao, 'indDesoneracao', '0'));
        infoContribuinte.infoCadastro.indAcordoIsenMulta := StrToindAcordoIsenMulta(Ok, INIRec.ReadString(sSecao, 'indAcordoIsenMulta', '0'));
        infoContribuinte.infoCadastro.indSitPJ           := StrToindSitPJ(Ok, INIRec.ReadString(sSecao, 'indSitPJ', '0'));

        sSecao := 'contato';
        infoContribuinte.infoCadastro.Contato.NmCtt    := INIRec.ReadString(sSecao, 'nmCtt', EmptyStr);
        infoContribuinte.infoCadastro.Contato.cpfCtt   := INIRec.ReadString(sSecao, 'cpfCtt', EmptyStr);
        infoContribuinte.infoCadastro.Contato.foneFixo := INIRec.ReadString(sSecao, 'foneFixo', EmptyStr);
        infoContribuinte.infoCadastro.Contato.foneCel  := INIRec.ReadString(sSecao, 'foneCel', EmptyStr);
        infoContribuinte.infoCadastro.Contato.email    := INIRec.ReadString(sSecao, 'email', EmptyStr);

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'softHouse' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'cnpjSoftHouse', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoContribuinte.infoCadastro.SoftwareHouse.Add do
          begin
            CnpjSoftHouse := sFim;
            nmRazao       := INIRec.ReadString(sSecao, 'nmRazao', '');
            nmCont        := INIRec.ReadString(sSecao, 'nmCont', '');
            telefone      := INIRec.ReadString(sSecao, 'telefone', '');
            email         := INIRec.ReadString(sSecao, 'email', '');
          end;

          Inc(I);
        end;

        sSecao := 'infoEFR';
        if INIRec.ReadString(sSecao, 'ideEFR', '') <> '' then
        begin
          infoContribuinte.infoCadastro.infoEFR.ideEFR  := StrToSimNao(Ok, INIRec.ReadString(sSecao, 'ideEFR', 'S'));
          infoContribuinte.infoCadastro.infoEFR.cnpjEFR := INIRec.ReadString(sSecao, 'cnpjEFR', EmptyStr);
        end;

        if ModoLancamento = toAlteracao then
        begin
          sSecao := 'novaValidade';
          infoContribuinte.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoContribuinte.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
        end;
      end;
    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

end.
