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
|* 04/10/2005: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrBAL
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALClass;

interface
uses ACBrDevice,      {Units da ACBr}
     Classes,
     {$IFDEF COMPILER6_UP} Types {$ELSE} Windows {$ENDIF} ;

type

{ Classe generica de BALANCA, nao implementa nenhum modelo especifico, apenas
  declara a Classe. NAO DEVE SER INSTANCIADA. Usada apenas como base para
  as demais Classes de BALANCA como por exemplo a classe TACBrBALFilizola }

{ TACBrBALClass }

TACBrBALClass = class
  private
    procedure SetAtivo(const Value: Boolean);
  protected
    fpDevice  : TACBrDevice ;
    fpAtivo   : Boolean ;
    fpModeloStr: String;
    fpUltimoPesoLido: Double;
    fpUltimaResposta: AnsiString;
    fpArqLOG: String;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy  ; override ;

    property Ativo  : Boolean read fpAtivo write SetAtivo ;
    procedure Ativar ; virtual ;
    procedure Desativar ; virtual ;

    function LePeso( MillisecTimeOut : Integer = 3000) :Double ; virtual;
    procedure LeSerial( MillisecTimeOut : Integer = 500) ; virtual ;
    procedure GravaLog(AString: AnsiString; Traduz :Boolean = True);

    property ModeloStr: String  read fpModeloStr ;
    property UltimoPesoLido : Double read fpUltimoPesoLido ;
    property UltimaResposta : AnsiString read fpUltimaResposta ;
    property ArqLOG : String read fpArqLOG write fpArqLOG ;
end;

implementation

Uses
  ACBrBAL, ACBrUtil, 
  SysUtils;

{ TACBrBALClass }

constructor TACBrBALClass.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrBAL) then
     raise Exception.create(ACBrStr('Essa Classe deve ser instanciada por TACBrBAL'));

  { Criando ponteiro interno para as Propriedade SERIAL de ACBrBAL,
    para permitir as Classes Filhas o acesso a essas propriedades do Componente}

  fpDevice    := (AOwner as TACBrBAL).Device ;
  fpDevice.SetDefaultValues ;

  fpAtivo     := false ;
  fpModeloStr := 'Não Definida' ;
  fpArqLOG    := '' ;
end;

destructor TACBrBALClass.Destroy;
begin
  fpDevice := nil ; { Apenas remove referencia (ponteiros internos) }

  inherited Destroy;
end;

procedure TACBrBALClass.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrBALClass.Ativar;
begin
  if fpAtivo then exit ;

  GravaLog( sLineBreak +
            StringOfChar('-',80)+ sLineBreak +
            'ATIVAR - '+FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now)+
            ' - Modelo: '+ModeloStr+
            ' - Porta: '+fpDevice.Porta+
            '         Device: '+fpDevice.DeviceToString(False) + sLineBreak +
            StringOfChar('-',80) + sLineBreak, False );

  if fpDevice.Porta <> '' then
     fpDevice.Ativar ;

  fpAtivo          := true ;
  fpUltimaResposta := '' ;
  fpUltimoPesoLido := 0 ;
end;

procedure TACBrBALClass.Desativar;
begin
  if not fpAtivo then exit ;

  if fpDevice.Porta <> '' then
     fpDevice.Desativar ;

  fpAtivo := false ;
end;

function TACBrBALClass.LePeso( MillisecTimeOut : Integer): Double;
begin
  { Deve ser implementada na Classe Filha }
  raise Exception.Create(ACBrStr('Função LePeso não implementada em: ')+ModeloStr);
end;

procedure TACBrBALClass.LeSerial( MillisecTimeOut : Integer) ;
begin
  { Deve ser implementada na Classe Filha }
  raise Exception.Create(ACBrStr('Procedure LeSerial não implementada em: ')+ModeloStr);
end;

procedure TACBrBALClass.GravaLog(AString: AnsiString; Traduz: Boolean);
begin
  WriteLog(fpArqLOG,AString,Traduz);
end;

end.
