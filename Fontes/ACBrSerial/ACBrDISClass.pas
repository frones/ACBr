{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
|* 28/09/2004: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrDIS
******************************************************************************}

{$I ACBr.inc}

unit ACBrDISClass;

interface
uses ACBrDevice,      {Units da ACBr}
     SysUtils,
     Classes,
     {$IFDEF COMPILER6_UP} Types {$ELSE} Windows {$ENDIF} ;

const
   PortAtOut = 96 ;       // Hexadecimal = 60
   PortAtIn  = 100 ;      //  Hexadecimal = 64

type

EACBrDISErro            = class(Exception) ;
  EACBrDISNaoSuportaLimparLinha = class(EACBrDISErro) ;

{ Classe generica de DISPLAY, nao implementa nenhum modelo especifico, apenas
  declara a Classe. NAO DEVE SER INSTANCIADA. Usada apenas como base para
  as demais Classes de DISPLAY como por exemplo a classe TACBrDISGertecSerial }

{ TACBrDISClass }

TACBrDISClass = class
  private

    fsColunas: Integer;
    fsLinhasCount: Integer;

    procedure SetAtivo(const Value: Boolean);
    procedure SetColunas(const Value: Integer);
  protected
    fpDevice  : TACBrDevice ;

    fpAtivo   : Boolean ;
    fpModeloStr: String;
    fpPassos: Integer;
    fpIntervaloEnvioBytes: Integer;

    procedure WaitForKeyBoard ;
    procedure TxKeyboard(B: Byte); overload;

  public
    Cursor : TPoint ;

    constructor Create(AOwner: TComponent); virtual;
    Destructor Destroy  ; override ;

    Property Ativo  : Boolean read fpAtivo write SetAtivo ;
    procedure Ativar ; virtual ;
    procedure Desativar ; virtual ;

    Property ModeloStr: String  read fpModeloStr ;

    property LinhasCount : Integer read fsLinhasCount  write fsLinhasCount  ;
    property Colunas : Integer read fsColunas write SetColunas ;
    property IntervaloEnvioBytes : Integer read fpIntervaloEnvioBytes
        write fpIntervaloEnvioBytes ;

    procedure LimparDisplay ; virtual ;
    procedure LimparLinha( Linha: Integer ) ; virtual ;

    procedure PosicionarCursor( Linha, Coluna: Integer ) ; virtual ;
    procedure Escrever( const Texto : String ) ; virtual ;
end ;

implementation
Uses ACBrDIS, ACBrUtil;

{ TACBrDISClass }

constructor TACBrDISClass.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrDIS) then
     raise Exception.create(ACBrStr('Essa Classe deve ser instanciada por TACBrDIS'));

  { Criando ponteiro interno para as Propriedade SERIAL de ACBrDIS,
    para permitir as Classes Filhas o acesso a essas propriedades do Componente}

  fsLinhasCount := 2 ;
  fsColunas     := 20 ;
         
  fpDevice    := (AOwner as TACBrDIS).Device ;
  fpDevice.SetDefaultValues ;

  fpAtivo     := false ;
  fpModeloStr := 'Não Definida' ;
  fpPassos    := 1 ;
  fpIntervaloEnvioBytes := 0 ;
end;

destructor TACBrDISClass.Destroy;
begin
  fpDevice := nil ; { Apenas remove referencia (ponteiros internos) }

  inherited Destroy;
end;

procedure TACBrDISClass.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrDISClass.Ativar;
begin
  if fpAtivo then exit ;

  if Trim(fpDevice.Porta) <> '' then
     fpDevice.Ativar ;
     
  fpAtivo := true ;
end;

procedure TACBrDISClass.Desativar;
begin
  if not fpAtivo then exit ;

  if Trim(fpDevice.Porta) <> '' then
     fpDevice.Desativar ;
     
  fpAtivo := false ;
end;

procedure TACBrDISClass.SetColunas(const Value: Integer);
begin
  fsColunas := Value;
end;

procedure TACBrDISClass.LimparDisplay;
begin
  { Deve ser implementada na ClassFilha }
end;

procedure TACBrDISClass.LimparLinha(Linha: Integer);
begin
  { Pode ser implementada na ClassFilha }
  PosicionarCursor(Linha, 1);
  Escrever( StringOfChar(' ',Colunas) );
end;

procedure TACBrDISClass.PosicionarCursor(Linha, Coluna: Integer);
begin
  { Deve ser implementada na ClassFilha }
end;

procedure TACBrDISClass.Escrever(const Texto: String);
begin
  { Deve ser implementada na ClassFilha }
end;

procedure TACBrDISClass.WaitForKeyBoard;
var
   I, MaxLoops: Integer;
begin
  { Aguarda se a porta AT nao está livre }

  I := 0 ;
  MaxLoops := 10000;
  if fpIntervaloEnvioBytes > 0 then
     MaxLoops := trunc( 1000 / fpIntervaloEnvioBytes );  // Até 1 seg

  while ((InPort( PortAtIn ) and 2) <> 0) and (I < MaxLoops) do
  begin
     if fpIntervaloEnvioBytes > 0 then
        sleep( fpIntervaloEnvioBytes ) ;
     Inc(I) ;
  end ;
end;

procedure TACBrDISClass.TxKeyboard(B: Byte);
begin
  WaitForKeyBoard;

  OutPort( PortAtOut, B);
end;

end.
