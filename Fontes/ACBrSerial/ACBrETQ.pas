{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2007 Andrews Ricardo Bejatto                }
{                                       Anderson Rogerio Bejatto               }
{                                                                              }
{ Colaboradores nesse arquivo:          Daniel Simooes de Almeida              }
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
|* 27/03/2007: Andrews R Bejatto/ Anderson R Bejatto/ Daniel Simões de Almeida
|*  - Primeira Versao ACBrETQ
|* 17/04/2009: Alexsander da Rosa
|*  - Parametro "SubFonte" na procedure ImprimirTexto
|* 29/05/2010: Alexsander da Rosa
|*  - Propriedade "Unidade" para indicar milimetros/polegadas
******************************************************************************}

{$I ACBr.inc}

unit ACBrETQ;

interface
uses ACBrBase, ACBrDevice, ACBrETQClass,  {Units da ACBr}
     Classes, SysUtils;

type

TACBrETQModelo = (etqNenhum, etqPpla, etqPplb, etqZPLII, etqEpl2);

{ TACBrETQ }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
TACBrETQ = class( TACBrComponent )
  private
    fsDevice  : TACBrDevice ;   { SubComponente ACBrDevice }
    { Propriedades do Componente ACBrETQ }
    fsAtivo  : Boolean;
    fsModelo : TACBrETQModelo;
    fsETQ    : TACBrETQClass ;

    function GetArqLOG: String;
    function GetLimparMemoria : Boolean;
    function GetOnGravarLog: TACBrGravarLog;
    procedure SetArqLOG(AValue: String);
    procedure SetLimparMemoria(const AValue : Boolean);
    procedure SetModelo(const Value: TACBrETQModelo);
    procedure SetOnGravarLog(AValue: TACBrGravarLog);
    procedure SetPorta(const Value: String);
    procedure SetAtivo(const Value: Boolean);

    function GetPorta: String;
    function GetModeloStrClass: String;
    procedure SetTemperatura(const Value: Integer);
    function GetTemperatura: Integer;
    function GetAvanco: Integer;
    procedure SetAvanco(const AValue: Integer);
    function GetUnidade: TACBrETQUnidade;
    procedure SetUnidade(const AValue: TACBrETQUnidade);
    function GetDPI : TACBrETQDPI ;
    procedure SetDPI(const AValue : TACBrETQDPI) ;
    function GetAdicionarComandoP: Boolean;
    procedure SetAdicionarComandoP(const Value: Boolean);
    function GetEtqFinalizada: Boolean;
    function GetEtqInicializada: Boolean;
  protected

  public
    property Ativo : Boolean read fsAtivo write SetAtivo ;
    property ETQ : TACBrETQClass read fsETQ;
    property ModeloStr : String read GetModeloStrClass;
    property EtqInicializada: Boolean read GetEtqInicializada;
    property EtqFinalizada: Boolean read GetEtqFinalizada;

    constructor Create(AOwner: TComponent); override;
    Destructor Destroy  ; override;

    procedure Ativar ;
    procedure Desativar ;

    procedure ImprimirTexto(Orientacao: TACBrETQOrientacao; Fonte, MultiplicadorH,
      MultiplicadorV, Vertical, Horizontal: Integer; Texto: String;
      SubFonte: Integer = 0; ImprimirReverso : Boolean = False);
    procedure ImprimirBarras(Orientacao: TACBrETQOrientacao; TipoBarras,
      LarguraBarraLarga, LarguraBarraFina: String; Vertical, Horizontal: Integer;
      Texto: String; AlturaCodBarras: Integer = 0;
      ExibeCodigo: TACBrETQBarraExibeCodigo = becPadrao);
    procedure ImprimirLinha(Vertical, Horizontal, Largura, Altura: Integer);
    procedure ImprimirCaixa(Vertical, Horizontal, Largura, Altura,
      EspessuraVertical, EspessuraHorizontal: Integer);
    procedure ImprimirImagem(MultiplicadorImagem, Vertical, Horizontal: Integer;
       NomeImagem: String);
    procedure CarregarImagem(AStream : TStream; NomeImagem: String;
       Flipped : Boolean = True; Tipo: String = 'BMP' ); overload;
    procedure CarregarImagem(ArquivoImagem, NomeImagem: String;
       Flipped : Boolean = True  ); overload;
    procedure IniciarEtiqueta;
    procedure FinalizarEtiqueta(Copias: Integer = 1; AvancoEtq: Integer = 0);
    procedure Imprimir(Copias: Integer = 1; AvancoEtq: Integer = 0);

  published
    property Modelo : TACBrETQModelo read fsModelo write SetModelo
      default etqNenhum ;
    property Unidade : TACBrETQUnidade read GetUnidade write SetUnidade
      default etqMilimetros;
    property DPI: TACBrETQDPI read GetDPI write SetDPI default dpi203;
    property Porta : String read GetPorta write SetPorta ;
    property Temperatura: Integer read GetTemperatura write SetTemperatura
      default 10 ;
    property Avanco: Integer read GetAvanco write SetAvanco
      default 0 ;
    property LimparMemoria: Boolean read GetLimparMemoria write SetLimparMemoria
      default True ;

   property AdicionarComandoP: Boolean read GetAdicionarComandoP write SetAdicionarComandoP
     default False;

   property ArqLOG: String read GetArqLOG write SetArqLOG;
   property OnGravarLog: TACBrGravarLog read GetOnGravarLog write SetOnGravarLog;

    { Instancia do Componente ACBrDevice, será passada para fsETQ.create }
    property Device : TACBrDevice read fsDevice ;
  end ;

implementation
Uses ACBrUtil, ACBrETQPpla, ACBrETQZplII, ACBrETQEpl2,
     {$IFDEF COMPILER6_UP} StrUtils {$ELSE} ACBrD5{$ENDIF};

{ TACBrETQ }

constructor TACBrETQ.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  fsAtivo       := false ;
  fsModelo      := etqNenhum ;

  { Instanciando SubComponente TACBrDevice }
  fsDevice := TACBrDevice.Create( self ) ;  { O dono é o proprio componente }
  fsDevice.Name := 'ACBrDevice' ;      { Apenas para aparecer no Object Inspector}
  {$IFDEF COMPILER6_UP}
  fsDevice.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}
  fsDevice.Porta := 'LPT1';
  fsDevice.Serial.DeadlockTimeout := 1000 ;

  { Instanciando fsETQ com modelo Generico (TACBrETQClass) }
  fsETQ := TACBrETQClass.create( self ) ;
end;

destructor TACBrETQ.Destroy;
begin
  Desativar ;

  if Assigned(fsETQ) then
     FreeAndNil(fsETQ) ;

  FreeAndNil(fsDevice) ;

  inherited Destroy;
end;

procedure TACBrETQ.SetModelo(const Value: TACBrETQModelo);
var
  wTemperatura: Integer;
  wAvanco: Integer;
  wUnidade: TACBrETQUnidade;
  wDPI: TACBrETQDPI;
  wOnGravarLog: TACBrGravarLog;
  wArqLog: String;
begin
  if fsModelo = Value then Exit;

  wTemperatura := Temperatura;
  wAvanco      := Avanco;
  wUnidade     := Unidade;
  wDPI         := DPI;
  wOnGravarLog := OnGravarLog;
  wArqLog      := ArqLOG;

  if fsAtivo then
     raise Exception.Create(ACBrStr('Não é possível mudar o Modelo com ACBrETQ Ativo'));

  FreeAndNil(fsETQ) ;

  { Instanciando uma nova classe de acordo com fsModelo }
  case Value of
     etqPpla          : fsETQ := TACBrETQPpla.create( Self ) ;
     etqPplb, etqEpl2 : fsETQ := TACBrETQEpl2.Create( self );  // EPL2 = PPLB
     etqZPLII         : fsETQ := TACBrETQZplII.Create ( Self );
  else
     fsETQ := TACBrETQClass.create( Self ) ;
  end;

  Temperatura := wTemperatura ;
  Avanco      := wAvanco ;
  Unidade     := wUnidade;
  DPI         := wDPI;
  OnGravarLog := wOnGravarLog;
  ArqLOG      := wArqLog;

  fsModelo := Value;
end;

procedure TACBrETQ.SetOnGravarLog(AValue: TACBrGravarLog);
begin
  fsETQ.OnGravarLog := AValue;
end;

procedure TACBrETQ.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrETQ.Ativar;
begin
  if fsAtivo then exit ;

  fsETQ.Ativar ;
  fsAtivo := true ;
end;

procedure TACBrETQ.Desativar;
begin
  if not fsAtivo then exit ;

  fsETQ.Desativar ;
  fsAtivo := false;
end;


function TACBrETQ.GetModeloStrClass: String;
begin
  Result := ACBrStr(fsETQ.ModeloStr) ;
end;

function TACBrETQ.GetPorta: String;
begin
  result := fsDevice.Porta ;
end;

procedure TACBrETQ.SetPorta(const Value: String);
begin
  fsDevice.Porta := Value ;
end;

function TACBrETQ.GetLimparMemoria : Boolean;
begin
   Result := fsETQ.LimparMemoria;
end;

function TACBrETQ.GetArqLOG: String;
begin
  Result := fsETQ.ArqLOG;
end;

function TACBrETQ.GetOnGravarLog: TACBrGravarLog;
begin
  Result := fsETQ.OnGravarLog;
end;

procedure TACBrETQ.SetArqLOG(AValue: String);
begin
  fsETQ.ArqLOG := AValue;
end;

function TACBrETQ.GetEtqFinalizada: Boolean;
begin
  Result := fsETQ.EtqFinalizada;
end;

function TACBrETQ.GetEtqInicializada: Boolean;
begin
  Result := fsETQ.EtqInicializada;
end;

procedure TACBrETQ.SetLimparMemoria(const AValue : Boolean);
begin
   fsETQ.LimparMemoria := AValue;
end;

procedure TACBrETQ.ImprimirCaixa(Vertical, Horizontal, Largura, Altura,
  EspessuraVertical, EspessuraHorizontal: Integer);
begin
  if not Ativo then
     Ativar ;

  fsETQ.ImprimirCaixa( Vertical, Horizontal,
                       Largura, Altura,
                       EspessuraVertical, EspessuraHorizontal);
end;

procedure TACBrETQ.ImprimirLinha(Vertical, Horizontal, Largura,
  Altura: Integer);
begin
  if not Ativo then
     Ativar ;

  fsETQ.ImprimirLinha(Vertical, Horizontal, Largura, Altura);
end;

procedure TACBrETQ.ImprimirTexto(Orientacao: TACBrETQOrientacao; Fonte, MultiplicadorH,
  MultiplicadorV, Vertical, Horizontal: Integer; Texto: String; SubFonte: Integer = 0;
  ImprimirReverso : Boolean = False);
begin
  if not Ativo then
     Ativar;

  if (Vertical < 0) then
     Raise Exception.Create(ACBrStr('Informe um valor positivo para Vertical'));

  if (Horizontal < 0) then
     Raise Exception.Create(ACBrStr('Informe um valor positivo para Horizontal'));

  fsETQ.ImprimirTexto( Orientacao, Fonte,
                       MultiplicadorH, MultiplicadorV,
                       Vertical, Horizontal,
                       Texto, SubFonte,
                       ImprimirReverso);
end;

procedure TACBrETQ.ImprimirBarras(Orientacao: TACBrETQOrientacao; TipoBarras,
  LarguraBarraLarga, LarguraBarraFina: String; Vertical, Horizontal: Integer;
  Texto: String; AlturaCodBarras: Integer;
  ExibeCodigo: TACBrETQBarraExibeCodigo);
begin
  if not Ativo then
     Ativar ;

  if (Vertical < 0) then
     Raise Exception.Create(ACBrStr('Informe um valor positivo para Vertical'));

  if (Horizontal < 0) then
     Raise Exception.Create(ACBrStr('Informe um valor positivo para Horizontal'));

  if (AlturaCodBarras < 0) then
     Raise Exception.Create(ACBrStr('Informe um valor positivo para AlturaCodBarras'));

  fsETQ.ImprimirBarras( Orientacao, TipoBarras,
                        LarguraBarraLarga, LarguraBarraFina,
                        Vertical, Horizontal, Texto,
                        AlturaCodBarras, ExibeCodigo);
end;

procedure TACBrETQ.Imprimir(Copias: Integer; AvancoEtq: Integer);
begin
  if not Ativo then
     Ativar ;

  fsETQ.Imprimir(Copias, AvancoEtq);
end;

procedure TACBrETQ.SetTemperatura(const Value: Integer);
begin
  fsETQ.Temperatura:= Value;
end;

function TACBrETQ.GetTemperatura: Integer;
begin
  Result := fsETQ.Temperatura;
end;

function TACBrETQ.GetAvanco: Integer;
begin
  Result := fsETQ.Avanco;
end;

procedure TACBrETQ.SetAvanco(const AValue: Integer);
begin
  fsETQ.Avanco := AValue;
end;

procedure TACBrETQ.ImprimirImagem(MultiplicadorImagem, Vertical, Horizontal:
  Integer; NomeImagem: String);
begin
  if not Ativo then
    Ativar;

  fsETQ.ImprimirImagem(MultiplicadorImagem, Vertical, Horizontal, NomeImagem);
end;

procedure TACBrETQ.CarregarImagem(AStream : TStream; NomeImagem: String;
   Flipped : Boolean; Tipo: String);
begin
  if not Ativo then
    Ativar;

  Tipo := UpperCase( RightStr( Tipo, 3) );

  fsETQ.CarregarImagem( AStream, NomeImagem, Flipped, Tipo );
end;

procedure TACBrETQ.CarregarImagem(ArquivoImagem, NomeImagem: String;
   Flipped : Boolean = True  );
var
   MS : TMemoryStream ;
   Tipo : String ;
begin
  if not FileExists( ArquivoImagem ) then
     raise Exception.Create( ACBrStr('Arquivo '+ArquivoImagem+' não encontrado'));

  Tipo := ExtractFileExt( ArquivoImagem );
  MS   := TMemoryStream.Create;
  try
     MS.LoadFromFile( ArquivoImagem );
     CarregarImagem( MS, NomeImagem, Flipped, Tipo);
  finally
     MS.Free;
  end ;
end ;

procedure TACBrETQ.IniciarEtiqueta;
begin
  if not Ativo then
    Ativar;

  fsETQ.IniciarEtiqueta;
end;

procedure TACBrETQ.FinalizarEtiqueta(Copias: Integer = 1; AvancoEtq: Integer = 0);
begin
  if not Ativo then
    Ativar;

  fsETQ.FinalizarEtiqueta(Copias, AvancoEtq);
end;

function TACBrETQ.GetUnidade: TACBrETQUnidade;
begin
  Result := fsETQ.Unidade ;
end;

procedure TACBrETQ.SetUnidade(const AValue: TACBrETQUnidade);
begin
  fsETQ.Unidade := AValue ;
end;

function TACBrETQ.GetDPI : TACBrETQDPI ;
begin
   Result := fsETQ.DPI;
end;

procedure TACBrETQ.SetDPI(const AValue : TACBrETQDPI) ;
begin
  fsETQ.DPI := AValue;
end;

function TACBrETQ.GetAdicionarComandoP: Boolean;
begin
  Result := fsETQ.AdicionarComandoP
end;

procedure TACBrETQ.SetAdicionarComandoP(const Value: Boolean);
begin
 fsETQ.AdicionarComandoP := Value;
end;

end.


