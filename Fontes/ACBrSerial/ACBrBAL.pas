{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Fabio Farias                           }
{                                       Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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

{******************************************************************************
|* Historico
|*
|* 04/10/2005: Fabio Farias  / Daniel Simões de Almeida
|*  - Primeira Versao ACBrBAL
******************************************************************************}

{$I ACBr.inc}

unit ACBrBAL;

interface

uses
  ACBrDevice, ACBrBase, ACBrBALClass,  {Units da ACBr}
  SysUtils, Classes,
  {$IFNDEF NOGUI}
    {$IFDEF VisualCLX}
      QExtCtrls,
    {$ELSE}
      ExtCtrls,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF COMPILER6_UP}
    Types
  {$ELSE}
    Windows
  {$ENDIF};

type

TACBrBALModelo = (balNenhum, balFilizola, balToledo, balToledo2090, balToledo2180, balUrano,
                  balLucasTec, balMagna, balDigitron, balMagellan, balUranoPOP, balLider,
                  balRinnert, balMuller, balSaturno, balAFTS, balGenerica, balLibratek,
                  balMicheletti, balAlfa, balToledo9091_8530_8540) ;
TACBrBALLePeso = procedure(Peso : Double; Resposta : AnsiString) of object ;

{ Componente ACBrBAL }

{ TACBrBAL }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
TACBrBAL = class(TACBrComponent)
  private
    fsDevice: TACBrDevice;  { SubComponente ACBrDevice }
    {$IFNDEF NOGUI}
      fsTimer: TTimer;
    {$ELSE}
      fsTimer: TACBrThreadTimer;
    {$ENDIF}

    { Propriedades do Componente ACBrBAL }
    fsAtivo  : Boolean;
    fsModelo : TACBrBALModelo;
    fsBAL    : TACBrBALClass ;
    fsOnLePeso: TACBrBALLePeso;
    fsMonitorarBalanca: Boolean;
    fsIntervalo: Integer;

    function GetArqLOG: String;
    procedure SetArqLOG(const AValue: String);
    procedure SetModelo(const Value: TACBrBALModelo);
    procedure SetPorta(const Value: String);
    procedure SetAtivo(const Value: Boolean);
    procedure SetPosIni(const Value: Integer);
    procedure SetPosFim(const Value: Integer);
    function GetPosIni: Integer;
    function GetPosFim: Integer;
    procedure LeSerial(Sender: TObject); virtual;

    function GetPorta: String;
    function GetModeloStrClass: String;
    function GetUltimoPesoLido: Double;
    procedure SetMonitorarBalanca(const Value: Boolean);
    procedure SetIntervalo(const Value: Integer);
    function GetUltimaResposta: AnsiString;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Ativar ;
    procedure Desativar ;

    function LePeso( MillisecTimeOut: Integer = 3000): Double;

    procedure SolicitarPeso;
    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; virtual;

    property UltimoPesoLido : Double read GetUltimoPesoLido ;
    property UltimaResposta : AnsiString read GetUltimaResposta ;
    property Ativo : Boolean read fsAtivo write SetAtivo ;
    property BAL : TACBrBALClass read fsBAL;
    property ModeloStr : String read GetModeloStrClass;
  published
     property Modelo : TACBrBALModelo read fsModelo write SetModelo
                 default balNenhum ;
     property Porta : String read GetPorta write SetPorta ;
     property Intervalo  : Integer      read fsIntervalo
        write SetIntervalo default 200 ;
     property MonitorarBalanca : Boolean read fsMonitorarBalanca
        write SetMonitorarBalanca default False ;
     property ArqLOG : String      read GetArqLOG write SetArqLOG ;
     property PosIni: Integer read GetPosini write SetPosIni default 0;
     property PosFim: Integer read GetPosFim write SetPosFim default 0;
     { Instancia do Componente ACBrDevice, será passada para fsBAL.create }
     property Device : TACBrDevice read fsDevice ;
     property OnLePeso : TACBrBALLePeso read fsOnLePeso write fsOnLePeso;
  end ;

implementation

uses
  ACBrUtil, ACBrBALFilizola, ACBrBALToledo, ACBrBALUrano, ACBrBALRinnert,
  ACBrBALMuller, ACBrBALLucasTec,  ACBrBALToledo2180, ACBrBALMagna,
  ACBrBALDigitron,ACBrBALMagellan, ACBrBALUranoPOP, ACBrBALLider,
  ACBrBALToledo2090, ACBrBALSaturno, ACBrBALAFTS, ACBrBALLibratek,
  ACBrBALMicheletti, ACBrBALAlfa, ACBrBALToledo9091_8530_8540,
  {$IFDEF COMPILER6_UP} StrUtils {$ELSE} ACBrD5{$ENDIF};

{ TACBrBAL }
constructor TACBrBAL.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  fsAtivo       := false ;
  fsModelo      := balNenhum ;
  fsIntervalo   := 200 ;

  { Instanciando SubComponente TACBrDevice }
  fsDevice := TACBrDevice.Create( self ) ;  { O dono é o proprio componente }
  fsDevice.Name := 'ACBrDevice' ;      { Apenas para aparecer no Object Inspector}
  {$IFDEF COMPILER6_UP}
  fsDevice.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}
  fsDevice.Porta := 'COM1';
  fsDevice.TimeOut := 1 ;

  { Timer para monitorar o envio de dados pela Balança }
  {$IFNDEF NOGUI}
    fsTimer := TTimer.Create(self) ;
  {$ELSE}
    fsTimer := TACBrThreadTimer.Create ;
  {$ENDIF}
  fsTimer.Enabled := False ;
  fsTimer.OnTimer := LeSerial ;

  { Instanciando fsBAL com modelo Generico (TACBrBALClass) }
  fsBAL := TACBrBALClass.create( self ) ;
end;

destructor TACBrBAL.Destroy;
begin
  Desativar;

  fsTimer.Enabled := False;
  fsTimer.Free;

  if Assigned(fsBAL) then
    FreeAndNil(fsBAL);

  FreeAndNil(fsDevice);

  inherited Destroy;
end;

procedure TACBrBAL.SetModelo(const Value: TACBrBALModelo);
var
  wArqLOG: String;
  wPosIni, wPosFim: Integer;
begin
  if (fsModelo = Value) then
    Exit;

  if fsAtivo then
    raise Exception.Create(ACBrStr('Não é possível mudar o Modelo com ACBrBAL Ativo'));

  wArqLOG := ArqLOG;
  wPosIni := 0;
  wPosFim := 0;

  if Assigned(fsBAL) then
  begin
    wPosIni := fsBAL.PosIni;
    wPosFim := fsBAL.PosFim;
  end;

  FreeAndNil(fsBAL);

  { Instanciando uma nova classe de acordo com fsModelo }
  case Value of
     balFilizola    : fsBAL := TACBrBALFilizola.create(Self);
     balToledo      : fsBAL := TACBrBALToledo.Create(Self);
     balToledo2180  : fsBAL := TACBrBALToledo2180.Create(Self);
     balUrano       : fsBAL := TACBrBALUrano.Create(Self);
     balLucasTec    : fsBAL := TACBrBALLucasTec.Create(Self);
     balMagna       : fsBAL := TACBrBALMagna.Create(Self);
     balDigitron    : fsBAL := TACBrBALDigitron.Create(Self);
     balMagellan    : fsBAL := TACBrBALMagellan.Create(Self);
     balUranoPOP    : fsBAL := TACBrBALUranoPOP.Create(Self);
     balLider       : fsBAL := TACBrBALLider.Create(Self);
     balToledo2090  : fsBAL := TACBrBALToledo2090.Create(Self);
     balRinnert     : fsBAL := TACBrBALRinnert.Create(Self);
     balMuller      : fsBAL := TACBrBALMuller.Create(Self);
     balSaturno     : fsBAL := TACBrBALSaturno.Create(Self);
     balAFTS        : fsBAL := TACBrBALAFTS.Create(Self);
     balGenerica    : fsBAL := TACBrBALClass.Create(Self);
     balLibratek    : fsBAL := TACBrBALLibratek.Create(Self);
     balMicheletti  : fsBAL := TACBrBALMicheletti.Create(Self);
     balAlfa        : fsBAL := TACBrBALAlfa.Create(Self);
     balToledo9091_8530_8540: fsBAL := TACBrBALToledo9091_8530_8540.Create(Self);
  else
     fsBAL := TACBrBALClass.Create(Self);
  end;

  fsBAL.PosIni := wPosIni;
  fsBAL.PosFim := wPosFim;
  ArqLOG       := wArqLOG;
  fsModelo     := Value;
end;

function TACBrBAL.GetArqLOG: String;
begin
  Result := fsBAL.ArqLOG;
end;

procedure TACBrBAL.SetArqLOG(const AValue: String);
begin
  fsBAL.ArqLOG := AValue;
end;

procedure TACBrBAL.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrBAL.Ativar;
begin
  if fsAtivo then exit ;

  fsBAL.Ativar ;
  fsAtivo   := true ;
  Intervalo := fsIntervalo ; { isso apenas verifica se precisa ligar o timer }
end;

procedure TACBrBAL.Desativar;
begin
  if not fsAtivo then exit ;

  fsTimer.Enabled := False ;
  fsBAL.Desativar ;
  fsAtivo := false;
end;


function TACBrBAL.GetModeloStrClass: String;
begin
  Result := ACBrStr(fsBAL.ModeloStr) ;
end;

function TACBrBAL.GetPorta: String;
begin
  result := fsDevice.Porta ;
end;

procedure TACBrBAL.SetPorta(const Value: String);
begin
  fsDevice.Porta := Value ;
end;

function TACBrBAL.LePeso( MillisecTimeOut : Integer) : Double;
Var
  Ativado, Monitorando : Boolean ;
begin
  Ativado     := Ativo ;
  Monitorando := MonitorarBalanca ;

  try
     MonitorarBalanca := False ;

     if not Ativado then   { Ativa caso não tenha sido ativado antes }
        Ativar ;

     Result := fsBAL.LePeso( MillisecTimeOut ) ;

     if Assigned( fsOnLePeso ) then
        fsOnLePeso( UltimoPesoLido, UltimaResposta ) ;
  finally
     Ativo            := Ativado ;
     MonitorarBalanca := Monitorando ; 
  end ;
end;

procedure TACBrBAL.SolicitarPeso;
begin
  fsBAL.SolicitarPeso;
end;

function TACBrBAL.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
begin
  Result := fsBAL.InterpretarRepostaPeso(aResposta);
end;

procedure TACBrBAL.LeSerial(Sender: TObject);  { Chamado pelo Timer interno }
begin
  fsTimer.Enabled := False;  { Desliga o Timer para evitar chamadas Recursivas }

  { Está ativo ? Tem dados esperando na porta Serial ? }
  if fsDevice.Ativo then
  begin
    if (fsDevice.BytesParaLer > 0) then
    begin
      fsBAL.LeSerial(500);

      if Assigned(fsOnLePeso) then
        fsOnLePeso(UltimoPesoLido, UltimaResposta);
    end;
  end;
  
  fsTimer.Enabled := True;
end;

function TACBrBAL.GetUltimoPesoLido: Double;
begin
  Result := fsBAL.UltimoPesoLido ;
end;

function TACBrBAL.GetUltimaResposta: AnsiString;
begin
  Result := fsBAL.UltimaResposta ;
end;

procedure TACBrBAL.SetMonitorarBalanca(const Value: Boolean);
begin
  fsMonitorarBalanca := Value;
  Intervalo := fsIntervalo ; { isso apenas verifica se precisa ligar o timer }
end;

procedure TACBrBAL.SetIntervalo(const Value: Integer);
begin
  fsTimer.Interval := Value ;
  fsIntervalo      := fsTimer.Interval ;
  fsTimer.Enabled  := fsMonitorarBalanca and fsAtivo and (fsIntervalo > 0) ;
end;

procedure TACBrBAL.SetPosFim(const Value: Integer);
begin
   fsBAL.PosFim := Value;
end;

procedure TACBrBAL.SetPosIni(const Value: Integer);
begin
   fsBAL.PosIni := Value;
end;

function TACBrBAL.GetPosIni: Integer;
begin
   Result := fsBAL.PosIni;
end;

function TACBrBAL.GetPosFim: Integer;
begin
   Result := fsBAL.PosFim
end;

end.


