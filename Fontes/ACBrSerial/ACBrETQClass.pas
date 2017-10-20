{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2007 Andrews Ricardo Bejatto                }
{                                       Anderson Rogerio Bejatto               }
{                                                                              }
{ Colaboradores nesse arquivo:          Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/03/2007: Andrews R Bejatto/ Anderson R Bejatto/ Daniel Simões de Almeida
|*  - Primeira Versao ACBrETQClass
|* 17/04/2009: Alexsander da Rosa
|*  - Parametro "SubFonte" na procedure ImprimirTexto
|* 29/05/2010: Alexsander da Rosa
|*  - Propriedade "Unidade" para indicar milimetros/polegadas
******************************************************************************}

{$I ACBr.inc}

unit ACBrETQClass;

interface
uses ACBrDevice, ACBrBase, Classes;

type

{ Classe generica de ETIQUETA, nao implementa nenhum modelo especifico, apenas
  declara a Classe. NAO DEVE SER INSTANCIADA. Usada apenas como base para
  as demais Classes de Impressora como por exemplo a classe TACBrETQPpla }

{ TACBrETQClass }

TACBrETQClass = class
  private
    fpArqLOG: String;
    fpOnGravarLog: TACBrGravarLog;
    FTemperatura: Integer;
    FAvanco: Integer;
    FUnidade: TACBrETQUnidade;
    FDPI: TACBrETQDPI;
    FVelocidade: Integer;
    procedure SetAtivo(const Value: Boolean);
    procedure SetTemperatura(const Value: Integer);
    procedure SetAvanco(const Value: Integer);
    procedure SetVelocidade(const Value: Integer);

  protected
    fpDevice: TACBrDevice;
    fpAtivo: Boolean;
    fpBackFeed: TACBrETQBackFeed;
    fpModeloStr: String;
    fpListaCmd: TStringList;
    fpCmd: AnsiString;
    fpLimparMemoria : Boolean;
    fpEtqFinalizada: Boolean;
    fpEtqInicializada: Boolean;

    procedure SetUnidade(const AValue: TACBrETQUnidade); virtual;
    procedure SetDPI(const AValue : TACBrETQDPI) ; virtual;

    procedure IniciarImpressao(Copias: Integer = 1; AvancoEtq: Integer = 0); virtual;
    procedure EnviarImpressao; virtual;
    procedure FinalizarImpressao; virtual;
    procedure CalcularComandoAbertura; virtual;
    procedure CalcularComandoFinaliza(Copias: Integer = 1; AvancoEtq: Integer = 0); virtual;

  public
    property Ativo  : Boolean read fpAtivo write SetAtivo;
    property ModeloStr: String read fpModeloStr;
    property ListaCmd: TStringList read fpListaCmd write fpListaCmd;
    property EtqInicializada: Boolean read fpEtqInicializada;
    property EtqFinalizada: Boolean read fpEtqFinalizada;
    property Cmd: AnsiString read fpCmd write fpCmd;
    property Temperatura: Integer read FTemperatura write SetTemperatura;
    property Avanco: Integer read FAvanco write SetAvanco;
    property Unidade: TACBrETQUnidade read FUnidade write SetUnidade;
    property DPI: TACBrETQDPI read FDPI write SetDPI;
    property LimparMemoria: Boolean read fpLimparMemoria write fpLimparMemoria;
    property BackFeed: TACBrETQBackFeed read fpBackFeed write fpBackFeed;
    property Velocidade: Integer read fVelocidade write SetVelocidade;

    property ArqLOG: String read fpArqLOG write fpArqLOG;
    property OnGravarLog: TACBrGravarLog read fpOnGravarLog write fpOnGravarLog;

    constructor Create(AOwner: TComponent);
    destructor Destroy  ; override;

    procedure Ativar ; virtual;
    procedure Desativar ; virtual;

    procedure GravaLog(AString: AnsiString; Traduz :Boolean = False);
    function ConverterUnidade( UnidadeSaida: TACBrETQUnidade;
       AValue : Integer) : Integer ; virtual;

    procedure ImprimirTexto(Orientacao: TACBrETQOrientacao; Fonte, MultiplicadorH,
      MultiplicadorV, Vertical, Horizontal: Integer;
      Texto: String; SubFonte: Integer = 0; ImprimirReverso : Boolean = False); virtual;
    procedure ImprimirBarras(Orientacao: TACBrETQOrientacao; TipoBarras,
      LarguraBarraLarga, LarguraBarraFina: String; Vertical, Horizontal: Integer;
      Texto: String; AlturaCodBarras: Integer;
      ExibeCodigo: TACBrETQBarraExibeCodigo = becPadrao); virtual;
    procedure ImprimirLinha(Vertical, Horizontal, Largura, Altura: Integer); virtual;
    procedure ImprimirCaixa(Vertical, Horizontal, Largura, Altura,
      EspessuraVertical, EspessuraHorizontal: Integer); virtual;
    procedure ImprimirImagem(MultiplicadorImagem, Vertical, Horizontal: Integer;
       NomeImagem: String); virtual;
    procedure CarregarImagem(AStream : TStream; NomeImagem: String;
       Flipped : Boolean = True; Tipo: String = 'BMP' ); virtual;
    procedure IniciarEtiqueta;
    procedure FinalizarEtiqueta(Copias: Integer = 1; AvancoEtq: Integer = 0);
    procedure Imprimir(Copias: Integer = 1; AvancoEtq: Integer = 0);
end;

implementation

Uses
  ACBrETQ, ACBrUtil, 
  SysUtils, math;

{ TACBrBAETQClass }

constructor TACBrETQClass.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrETQ) then
     raise Exception.create(ACBrStr('Essa Classe deve ser instanciada por TACBrETQ'));

  { Criando ponteiro interno para as Propriedade SERIAL de ACBrETQ,
    para permitir as Classes Filhas o acesso a essas propriedades do Componente}

  fpDevice    := (AOwner as TACBrETQ).Device ;
  fpDevice.SetDefaultValues ;

  fpAtivo     := false ;
  fpModeloStr := 'Não Definida' ;
  fpListaCmd:= TStringList.Create;
  fpLimparMemoria := True ;
  fpEtqInicializada := False;
  fpEtqFinalizada := False;
  
  FAvanco      := 0;
  FVelocidade  := -1;
  FTemperatura := 10;
  FUnidade     := etqMilimetros;
  FDPI         := dpi203;
end;

destructor TACBrETQClass.Destroy;
begin
  fpDevice := nil; { Apenas remove referencia (ponteiros internos) }
  FreeAndNil(fpListaCmd);
  inherited Destroy;
end;

procedure TACBrETQClass.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrETQClass.Ativar;
begin
  if fpAtivo then exit ;

  GravaLog( sLineBreak + StringOfChar('-',80)+ sLineBreak +
            'ATIVAR - '+FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now)+
            ' - Modelo: '+ModeloStr+
            ' - Porta: ' +fpDevice.Porta+
            ' - Device: '+fpDevice.DeviceToString(False) + sLineBreak +
            StringOfChar('-',80) + sLineBreak );

  if fpDevice.Porta <> '' then
     fpDevice.Ativar ;

  fpAtivo := true ;
  fpEtqInicializada := False;
  fpEtqInicializada := False;
end;

procedure TACBrETQClass.Desativar;
begin
  if not fpAtivo then exit ;

  if fpDevice.Porta <> '' then
     fpDevice.Desativar ;

  fpAtivo := false ;
end;

procedure TACBrETQClass.GravaLog(AString: AnsiString; Traduz: Boolean);
var
  Tratado: Boolean;
begin
  Tratado := False;

  if Traduz then
    AString := TranslateUnprintable(AString);

  if Assigned( fpOnGravarLog ) then
    fpOnGravarLog( AString, Tratado);

  if not Tratado then
    WriteLog(fpArqLOG, '-- '+FormatDateTime('dd/mm hh:nn:ss:zzz',now)+' '+ AString);
end;

function TACBrETQClass.ConverterUnidade( UnidadeSaida : TACBrETQUnidade;
   AValue : Integer) : Integer ;
Var
  ValorReal, ValorFinal, DotsMM, DotsPI : Double ;
begin
  Result     := AValue;
  ValorFinal := AValue ;  // evita Warnings
  ValorReal  := AValue ;

  if UnidadeSaida = Unidade then
     exit ;

  case DPI of
    dpi300 :
      begin
        DotsMM := 12;
        DotsPI := 300;
      end ;

    dpi600 :
      begin
        DotsMM := 23.5;
        DotsPI := 600;
      end ;
  else
   begin
     DotsMM := 8;
     DotsPI := 203;
   end ;
  end ;

  case Unidade of
    etqMilimetros : ValorReal := AValue / 10 ;
    etqPolegadas  : ValorReal := AValue / 100;
  end ;

  case UnidadeSaida of
    etqMilimetros :
       begin
         case Unidade of
            etqPolegadas : ValorFinal := ValorReal * 25.4;
            etqDots      : ValorFinal := ValorReal / DotsMM ;
         end ;

         ValorFinal := ValorFinal * 10;
       end ;

    etqPolegadas :
       begin
         case Unidade of
            etqMilimetros : ValorFinal := ValorReal / 25.4;
            etqDots       : ValorFinal := ValorReal / DotsPI ;
         end ;

         ValorFinal := ValorFinal * 100;
       end ;

    etqDots :
       begin
         case Unidade of
            etqMilimetros : ValorFinal := ValorReal * DotsMM;
            etqPolegadas  : ValorFinal := ValorReal * DotsPI ;
         end ;
       end ;
  end ;

  Result := trunc( RoundTo( ValorFinal, 0 ) );
end ;

procedure TACBrETQClass.ImprimirBarras(Orientacao: TACBrETQOrientacao;
  TipoBarras, LarguraBarraLarga, LarguraBarraFina: String;
  Vertical, Horizontal: Integer; Texto: String; AlturaCodBarras: Integer;
  ExibeCodigo: TACBrETQBarraExibeCodigo = becPadrao);
begin
  raise Exception.Create(ACBrStr('Função ImprimirBarras não implementada em: ') + ModeloStr);
end;

procedure TACBrETQClass.ImprimirCaixa(Vertical, Horizontal, Largura,
  Altura, EspessuraVertical, EspessuraHorizontal: Integer);
begin
  raise Exception.Create(ACBrStr('Função ImprimirCaixa não implementada em: ') + ModeloStr);
end;

procedure TACBrETQClass.ImprimirLinha(Vertical, Horizontal, Largura,
  Altura: Integer);
begin
  raise Exception.Create(ACBrStr('Função ImprimirLinha não implementada em: ') + ModeloStr);
end;

procedure TACBrETQClass.ImprimirTexto(Orientacao: TACBrETQOrientacao; Fonte, MultiplicadorH,
  MultiplicadorV, Vertical, Horizontal: Integer; Texto: String;
  SubFonte: Integer = 0; ImprimirReverso : Boolean = False);
begin
  raise Exception.Create(ACBrStr('Função ImprimirTexto não implementada em: ') + ModeloStr);
end;

procedure TACBrETQClass.SetTemperatura(const Value: Integer);
begin
  FTemperatura := Value;
end;

procedure TACBrETQClass.SetAvanco(const Value: Integer);
begin
  FAvanco := Value;
end;

procedure TACBrETQClass.SetUnidade(const AValue: TACBrETQUnidade);
begin
  FUnidade := AValue;
end;

procedure TACBrETQClass.SetVelocidade(const Value: Integer);
begin
  fVelocidade := Value;
end;

procedure TACBrETQClass.SetDPI(const AValue : TACBrETQDPI) ;
begin
   FDPI := AValue ;
end ;

procedure TACBrETQClass.IniciarImpressao(Copias: Integer; AvancoEtq: Integer);
begin
  if not (EtqInicializada or EtqFinalizada) then
    IniciarEtiqueta;

  if not EtqFinalizada then
    FinalizarEtiqueta(Copias, AvancoEtq);
end;

procedure TACBrETQClass.EnviarImpressao;
begin
  GravaLog(ListaCmd.Text, True);

  fpDevice.EnviaString(ListaCmd.Text);
end;

procedure TACBrETQClass.FinalizarImpressao;
begin
  Cmd := '';
  ListaCmd.Clear;

  fpEtqInicializada := False;
  fpEtqFinalizada   := False;
end;

procedure TACBrETQClass.Imprimir(Copias: Integer; AvancoEtq: Integer);
begin
  IniciarImpressao(Copias, AvancoEtq);
  EnviarImpressao;
  FinalizarImpressao;
end;

procedure TACBrETQClass.ImprimirImagem(MultiplicadorImagem, Vertical, Horizontal
  : Integer; NomeImagem : String);
begin
  raise Exception.Create(ACBrStr('Função ImprimirImagem não implementada em: ') + ModeloStr);
end;

procedure TACBrETQClass.CarregarImagem(AStream : TStream; NomeImagem: String;
   Flipped : Boolean; Tipo: String);
begin
  raise Exception.Create(ACBrStr('Função CarregarImagem não implementada em: ') + ModeloStr);
end;

procedure TACBrETQClass.IniciarEtiqueta;
begin
  Cmd := '';

  { Calcula comando de Abertura e atribui à Cmd }
  CalcularComandoAbertura;

  if not (EtqInicializada or EtqFinalizada) then
    ListaCmd.Insert(0, Cmd)      //Se Etiqueta não foi iniciada, comandos incluídos no início
  else
    ListaCmd.Add(Cmd);           //Se Etiqueta foi iniciada, comandos são concatenados

  fpEtqInicializada := True;
  fpEtqFinalizada   := False;
end;

procedure TACBrETQClass.FinalizarEtiqueta(Copias: Integer; AvancoEtq: Integer);
begin
  Cmd := '';

  { Calcula comando de Finalização e atribui à Cmd }
  CalcularComandoFinaliza(Copias, AvancoEtq);

  ListaCmd.Add(Cmd);

  fpEtqFinalizada   := True;
  fpEtqInicializada := False;
end;

procedure TACBrETQClass.CalcularComandoAbertura;
begin
  // Sem Implementação
end;

procedure TACBrETQClass.CalcularComandoFinaliza(Copias: Integer;
  AvancoEtq: Integer);
begin
  // Sem Implementação
end;

end.
