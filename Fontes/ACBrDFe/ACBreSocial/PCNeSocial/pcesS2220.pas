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

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit pcesS2220;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnGerador, pcnConsts,
  pcesCommon, pcesConversaoeSocial, pcesGerador, pcnLeitor;

type
  TS2220CollectionItem = class;
  TevtMonit = class;
  TexMedOcup = class;
  TAso = class;
  TExameCollectionItem = class;
  TExameCollection = class;
  TRespMonit = class;
  TMedico = class;

  TS2220Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2220CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2220CollectionItem);
  public
    function Add: TS2220CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2220CollectionItem;
    property Items[Index: Integer]: TS2220CollectionItem read GetItem write SetItem; default;
  end;

  TS2220CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtMonit: TevtMonit;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtMonit: TevtMonit read FevtMonit write FevtMonit;
  end;

  TevtMonit = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FexMedOcup: TexMedOcup;

    { Geradores da classe }
    procedure GerarExame;
    procedure GerarMedico;
    procedure GerarExMedOcup;
    procedure GerarAso;
    procedure GerarRespMonit;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerXML : Boolean;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property exMedOcup: TexMedOcup read FexMedOcup write FexMedOcup;
  end;

  TexMedOcup = class(TObject)
  private
    FtpExameOcup: tpTpExameOcup;
    FAso: TAso;
    FRespMonit : TRespMonit;
  public
    property tpExameOcup: tpTpExameOcup read FtpExameOcup write FtpExameOcup;
    property Aso : TAso read FAso write FAso;
    property RespMonit : TRespMonit read FRespMonit write FRespMonit;

    constructor Create;
    destructor Destroy; override;
  end;

  TAso = class(TObject)
  private
    FDtAso: TDateTime;
    FResAso: tpResAso;
    FExame: TExameCollection;
    FMedico: TMedico;
  public
    constructor Create;
    destructor Destroy; override;

    property DtAso: TDateTime read FDtAso write FDtAso;
    property ResAso: tpResAso read FResAso write FResAso;
    property Exame: TExameCollection read FExame write FExame;
    property Medico: TMedico read FMedico write FMedico;
  end;

  TExameCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TExameCollectionItem;
    procedure SetItem(Index: Integer; const Value: TExameCollectionItem);
  public
    function Add: TExameCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TExameCollectionItem;
    property Items[Index: Integer]: TExameCollectionItem read GetItem write SetItem;
  end;

  TExameCollectionItem = class(TObject)
  private
    FDtExm: TDateTime;
    FProcRealizado: string;
    FObsProc: string;
    FOrdExame: tpOrdExame;
    FIndResult: tpIndResult;
  public
    property DtExm: TDateTime read FDtExm write FDtExm;
    property ProcRealizado: string read FProcRealizado write FProcRealizado;
    property obsProc: string read FObsProc write FObsProc;
    property ordExame: tpOrdExame read FOrdExame write FOrdExame;
    property indResult: tpIndResult read FIndResult write FIndResult;
  end;

  TRespMonit = class
  private
    FCPFResp: String;
    FNMResp: String;
    FNRCRM: String;
    FUFCRM: string;
  public
    property cpfResp: String read FCPFResp write FCPFResp;
    property nmResp: String read FNMResp write FNMResp;
    property nrCRM: String read FNRCRM write FNRCRM;
    property ufCRM: string read FUFCRM write FUFCRM;
  end;

  TMedico = class
  private
    FNmMed: string;
    FCPFMed : String;
    FNISMed : String;
    FnrCRM: String;
    FufCRM: string;
  public
    property cpfMed: String read FCPFMed write FCPFMed;
    property nisMed: String read FNISMed write FNISMed;
    property NmMed: String read FNmMed write FNmMed;
    property nrCRM: String read FnrCRM write FnrCRM;
    property ufCRM: string read FufCRM write FufCRM;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2220Collection }

function TS2220Collection.Add: TS2220CollectionItem;
begin
  Result := Self.New;
end;

function TS2220Collection.GetItem(Index: Integer): TS2220CollectionItem;
begin
  Result := TS2220CollectionItem(inherited Items[Index]);
end;

procedure TS2220Collection.SetItem(Index: Integer;
  Value: TS2220CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2220Collection.New: TS2220CollectionItem;
begin
  Result := TS2220CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2220CollectionItem }

constructor TS2220CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS2220;
  FevtMonit   := TevtMonit.Create(AOwner);
end;

destructor TS2220CollectionItem.Destroy;
begin
  FevtMonit.Free;

  inherited;
end;

{ TAso }

constructor TAso.Create;
begin
  inherited;
  FResAso := raNaoInformado;
  FExame  := TExameCollection.Create;
  FMedico := TMedico.Create;
end;

destructor TAso.Destroy;
begin
  FExame.Free;
  FMedico.Free;
  inherited;
end;

{ TExameCollection }

function TExameCollection.Add: TExameCollectionItem;
begin
  Result := Self.New;
end;

function TExameCollection.GetItem(Index: Integer): TExameCollectionItem;
begin
  Result := TExameCollectionItem(inherited Items[Index]);
end;

procedure TExameCollection.SetItem(Index: Integer;
  const Value: TExameCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TExameCollection.New: TExameCollectionItem;
begin
  Result := TExameCollectionItem.Create;

  Result.FOrdExame  := orNaoInformado;
  Result.FIndResult := irNaoInformado;

  Self.Add(Result);
end;

{ TevtMonit }

constructor TevtMonit.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo    := TIdeVinculo.Create;
  FexMedOcup     := TexMedOcup.Create;
end;

destructor TevtMonit.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FexMedOcup.Free;

  inherited;
end;

procedure TevtMonit.GerarAso;
begin
  Gerador.wGrupo('aso');

  Gerador.wCampo(tcDat, '', 'dtAso',  10, 10, 1, self.exMedOcup.Aso.DtAso);
  if (self.exMedOcup.Aso.ResAso <> raNaoInformado) then
    Gerador.wCampo(tcStr, '', 'resAso',  1,  1, 1, eSResAsoToStr(self.exMedOcup.Aso.ResAso));

  GerarExame;
  GerarMedico;

  Gerador.wGrupo('/aso');
end;

procedure TevtMonit.GerarMedico;
var
  NrOcorrNrCRM: Integer;
begin
  if (VersaoDF >= veS01_02_00) then
    NrOcorrNrCRM := 0
  else
    NrOcorrNrCRM := 1;

  Gerador.wGrupo('medico');

  if VersaoDF <= ve02_05_00 then
  begin
    if Trim(self.exMedOcup.Aso.Medico.cpfMed) <> '' then
      Gerador.wCampo(tcStr, '', 'cpfMed', 11, 11, 0, self.exMedOcup.Aso.Medico.cpfMed);
   
    if Trim(self.exMedOcup.Aso.Medico.nisMed) <> '' then
      Gerador.wCampo(tcStr, '', 'nisMed', 1, 11, 0, self.exMedOcup.Aso.Medico.nisMed);
  end;

  Gerador.wCampo(tcStr, '', 'nmMed', 1, 70, 1, self.exMedOcup.Aso.Medico.NmMed);
  Gerador.wCampo(tcStr, '', 'nrCRM', 1, 10, NrOcorrNrCRM, self.exMedOcup.Aso.Medico.nrCRM);
  Gerador.wCampo(tcStr, '', 'ufCRM', 2, 2, NrOcorrNrCRM, self.exMedOcup.Aso.Medico.ufCRM);

  Gerador.wGrupo('/medico');
end;

procedure TevtMonit.GerarExame;
var
  i: integer;
begin
  for i:= 0 to self.exMedOcup.Aso.Exame.Count-1 do
  begin
    Gerador.wGrupo('exame');

    Gerador.wCampo(tcDat, '', 'dtExm',         10,  10, 1, self.exMedOcup.Aso.Exame.Items[i].dtExm);
    Gerador.wCampo(tcStr, '', 'procRealizado',  1,   4, 1, self.exMedOcup.Aso.Exame.Items[i].procRealizado);
    Gerador.wCampo(tcStr, '', 'obsProc',        1, 999, 0, self.exMedOcup.Aso.Exame.Items[i].obsProc);

    if (self.exMedOcup.Aso.Exame.Items[i].ordExame <> orNaoInformado) then
      Gerador.wCampo(tcInt, '', 'ordExame', 1, 1, 1, eSOrdExameToStr(self.exMedOcup.Aso.Exame.Items[i].ordExame));

    if (self.exMedOcup.Aso.Exame.Items[i].indResult <> irNaoInformado) then
      Gerador.wCampo(tcInt, '', 'indResult', 1, 1, 0, eSIndResultToStr(self.exMedOcup.Aso.Exame.Items[i].indResult));

    Gerador.wGrupo('/exame');
  end;

  if self.exMedOcup.Aso.Exame.Count > 99 then
    Gerador.wAlerta('', 'exame', 'Lista de Exames', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TevtMonit.GerarExMedOcup;
  function DeveGerarRespMonit: Boolean;
  begin
    //layout 2.5 ou algum campo preenchido deve gerar grupo.
    Result := (VersaoDF <= ve02_05_00) or (
     (Trim(self.exMedOcup.RespMonit.nmResp) <> '') and
     (Trim(self.exMedOcup.RespMonit.nrCRM)  <> '') and
     (Trim(self.exMedOcup.RespMonit.ufCRM)  <> '')); 
  end;
begin
  Gerador.wGrupo('exMedOcup');

  Gerador.wCampo(tcStr, '', 'tpExameOcup',   1,  1, 1, eSTpExameOcupToStr(self.exMedOcup.FtpExameOcup));

  GerarASO;

  if DeveGerarRespMonit then
  begin
    GerarRespMonit;
  end;

  Gerador.wGrupo('/exMedOcup');
end;

procedure TevtMonit.GerarRespMonit;
begin
  Gerador.wGrupo('respMonit');

  if Trim(self.exMedOcup.RespMonit.cpfResp) <> '' then
    Gerador.wCampo(tcStr, '', 'cpfResp', 11, 11, 0, self.exMedOcup.RespMonit.cpfResp);

  Gerador.wCampo(tcStr, '', 'nmResp', 1, 70, 1, self.exMedOcup.RespMonit.nmResp);
  Gerador.wCampo(tcStr, '', 'nrCRM', 1, 10, 1, self.exMedOcup.RespMonit.nrCRM);
  Gerador.wCampo(tcStr, '', 'ufCRM', 2, 2, 1, self.exMedOcup.RespMonit.ufCRM);

  Gerador.wGrupo('/respMonit');
end;

function TevtMonit.GerarXML: boolean;
begin
  Result := inherited GerarXML;
  try
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtMonit');
    Gerador.wGrupo('evtMonit Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo);
    GerarExMedOcup;

    Gerador.wGrupo('/evtMonit');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtMonit');

//    Validar(schevtMonit);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TevtMonit.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtMonit';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideVinculo';
      ideVinculo.CpfTrab   := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
//      ideVinculo.NisTrab   := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);
      ideVinculo.Matricula := INIRec.ReadString(sSecao, 'matricula', EmptyStr);
      ideVinculo.codCateg  := INIRec.ReadInteger(sSecao, 'codCateg', 0);

      sSecao := 'aso';
      exMedOcup.aso.DtAso  := StringToDateTime(INIRec.ReadString(sSecao, 'dtAso', '0'));
      exMedOcup.aso.ResAso := eSStrToResAso(Ok, INIRec.ReadString(sSecao, 'resAso', '1'));

      sSecao := 'exMedOcup';
      exMedOcup.tpExameOcup  := eSStrToTpExameOcup(Ok, INIRec.ReadString(sSecao, 'tpExameOcup', '0'));

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'exame' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'dtExm', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with exMedOcup.aso.exame.New do
        begin
          dtExm         := StringToDateTime(sFim);
          ProcRealizado := INIRec.ReadString(sSecao, 'procRealizado', EmptyStr);
          obsProc       := INIRec.ReadString(sSecao, 'obsProc', EmptyStr);
          ordExame      := eSStrToOrdExame(Ok, INIRec.ReadString(sSecao, 'ordExame', '1'));
          indResult     := eSStrToIndResult(Ok, INIRec.ReadString(sSecao, 'indResult', '1'));
        end;

        Inc(I);
      end;


      // I vai vir com o o valor do último exame + 1
      sSecao := 'respMonit'; // + IntToStrZero(I-1, 2);
      exMedOcup.RespMonit.cpfResp := INIRec.ReadString(sSecao, 'cpfResp', EmptyStr);
      exMedOcup.RespMonit.nmResp := INIRec.ReadString(sSecao, 'nmResp', EmptyStr);
      exMedOcup.RespMonit.nrCRM := INIRec.ReadString(sSecao, 'nrCRM', EmptyStr);
      exMedOcup.RespMonit.ufCRM := INIRec.ReadString(sSecao, 'ufCRM', 'SP');


      sSecao := 'medico';
      exMedOcup.Aso.medico.cpfMed := INIRec.ReadString(sSecao, 'cpfMed', EmptyStr);
      exMedOcup.Aso.medico.nisMed := INIRec.ReadString(sSecao, 'nisMed', EmptyStr);
      exMedOcup.Aso.medico.NmMed := INIRec.ReadString(sSecao, 'nmMed', EmptyStr);
      exMedOcup.Aso.medico.nrCRM := INIRec.ReadString(sSecao, 'nrCRM', EmptyStr);
      exMedOcup.Aso.medico.ufCRM := INIRec.ReadString(sSecao, 'ufCRM', 'SP');
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

{ TexMedOcup }

constructor TexMedOcup.Create;
begin
  inherited Create;
  FAso       := TAso.Create;
  FRespMonit := TRespMonit.Create;
end;

destructor TexMedOcup.Destroy;
begin
  FAso.Free;
  FRespMonit.Free;
  inherited;
end;

function TevtMonit.LerXML: Boolean;
var
  Leitor: TLeitor;
  ok: Boolean;
  i: integer;
begin
  Result := False;
  Leitor := TLeitor.Create;
  try
    Leitor.Arquivo := XML;

    if Leitor.rExtrai(1, 'evtMonit') <> '' then
    begin
      if Leitor.rExtrai(2, 'ideEvento') <> '' then
        with Self.ideEvento do
        begin
          indRetif := eSStrToIndRetificacao(ok, leitor.rCampo(tcStr, 'indRetif'));
          nrRecibo := Leitor.rCampo(tcStr,'nrRecibo');
          procEmi  := eSStrToprocEmi(ok, Leitor.rCampo(tcStr, 'procEmi'));
          verProc  := Leitor.rCampo(tcStr, 'verProc');
        end;

      if Leitor.rExtrai(2, 'ideEmpregador') <> '' then
        with Self.ideEmpregador do
        begin
          tpInsc := eSStrToTpInscricao(ok, Leitor.rCampo(tcStr, 'tpInsc'));
          nrInsc := Leitor.rCampo(tcStr, 'nrInsc');
        end;

      if Leitor.rExtrai(2, 'ideVinculo') <> '' then
        with Self.ideVinculo do
        begin
          cpfTrab   := Leitor.rCampo(tcStr, 'cpfTrab');
          matricula := Leitor.rCampo(tcStr, 'matricula');
          codCateg  := Leitor.rCampo(tcInt, 'codCateg');
        end;

      if Leitor.rExtrai(2, 'exMedOcup') <> '' then
      begin
        with Self.exMedOcup do
        begin
          tpExameOcup := eSStrToTpExameOcup(ok, Leitor.rCampo(tcStr, 'tpExameOcup'));

          if Leitor.rExtrai(3, 'aso') <> '' then
          begin
            with aso do
            begin
              dtAso  := Leitor.rCampo(tcDat, 'dtAso');
              resAso := eSStrToResAso(ok, Leitor.rCampo(tcStr, 'resAso'));

              i := 0;
              while Leitor.rExtrai(4, 'exame', '', i + 1) <> '' do
              begin
                with exame.New do
                begin
                  dtExm         := Leitor.rCampo(tcDat, 'dtExm');
                  procRealizado := Leitor.rCampo(tcStr, 'procRealizado');
                  obsProc       := Leitor.rCampo(tcStr, 'obsProc');
                  ordExame      := eSStrToOrdExame(ok, Leitor.rCampo(tcStr, 'ordExame'));
                  indResult     := eSStrToIndResult(ok, Leitor.rCampo(tcStr, 'indResult'));
                end;

                Inc(i);
              end;

              if Leitor.rExtrai(4, 'medico') <> '' then
                with medico do
                begin
                  nmMed := Leitor.rCampo(tcStr, 'nmMed');
                  nrCRM := Leitor.rCampo(tcStr, 'nrCRM');
                  ufCRM := Leitor.rCampo(tcStr, 'ufCRM');
                end;
            end;
          end;

          if Leitor.rExtrai(3, 'respMonit') <> '' then
            with respMonit do
            begin
              cpfResp := Leitor.rCampo(tcStr, 'cpfResp');
              nmResp  := Leitor.rCampo(tcStr, 'nmResp');
              nrCRM   := Leitor.rCampo(tcStr, 'nrCRM');
              ufCRM   := Leitor.rCampo(tcStr, 'ufCRM');
            end;
        end;
      end;

      Result := True;
    end;
  finally
    Leitor.Free;
  end;
end;

end.

