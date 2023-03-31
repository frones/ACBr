{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Fabio Farias                                    }
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

unit ACBrSINClass;

interface

uses
  SysUtils, Classes,

  {$IFDEF COMPILER6_UP}
   Types
  {$ELSE}
   Windows
  {$ENDIF},
  ACBrDevice
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type
  TACBrCorLed = (corNenhuma, corVerde, corAmarelo, corVermelho);

{ TACBrSINClass }

TACBrSINClass = class
  private
    procedure SetAtivo(const Value: Boolean);
  protected
    fpDevice: TACBrDevice;
    fpAtivo: Boolean;
    fpModeloStr: String;
    fpArqLOG: String;

    procedure GravaLog(const AString: AnsiString; Traduz: Boolean = True);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Ativar; virtual;
    procedure Desativar; virtual;

    procedure defineLed(const ALed: TACBrCorLed); virtual;
    procedure enviarSerial(cmd: AnsiString ); virtual;

    property ModeloStr: String  read fpModeloStr;
    property Ativo    : Boolean read fpAtivo  write SetAtivo;
    property ArqLOG   : String  read fpArqLOG write fpArqLOG;
end;

implementation

uses
  math,
  dateutils,
  strutils,
  ACBrSIN,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrConsts;

{ TACBrBALClass }

constructor TACBrSINClass.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrSIN) then
    raise Exception.create(ACBrStr('Essa Classe deve ser instanciada por TACBrSIN'));

  { Criando ponteiro interno para as Propriedade SERIAL de ACBrSIN,
    para permitir as Classes Filhas o acesso a essas propriedades do Componente}

  fpDevice := (AOwner as TACBrSIN).Device;
  fpDevice.SetDefaultValues;

  fpAtivo     := False;
  fpArqLOG    := '';
  fpModeloStr := 'Generica';
end;

destructor TACBrSINClass.Destroy;
begin
  fpDevice := Nil; { Apenas remove referencia (ponteiros internos) }

  inherited Destroy;
end;

procedure TACBrSINClass.SetAtivo(const Value: Boolean);
begin
  if Value then
    Ativar
  else
    Desativar;
end;

procedure TACBrSINClass.GravaLog(const AString: AnsiString; Traduz: Boolean);
begin
  WriteLog(fpArqLOG, AString, Traduz);
end;

procedure TACBrSINClass.Ativar;
begin
  if fpAtivo then
    Exit;

  GravaLog( sLineBreak   + StringOfChar('-',80)+ sLineBreak +
            'ATIVAR - '  + FormatDateTime('dd/mm/yy hh:nn:ss:zzz', Now) +
            ' - Modelo: '+ ModeloStr +
            ' - Porta: ' + fpDevice.Porta + '         Device: ' +
            fpDevice.DeviceToString(False) + sLineBreak +
            StringOfChar('-', 80) + sLineBreak, False);

  if (fpDevice.Porta <> '') then
    fpDevice.Ativar;

  fpAtivo := True;
end;

procedure TACBrSINClass.defineLed(const ALed: TACBrCorLed);
begin
  if not(fpAtivo) then
    Ativar;

  GravaLog( sLineBreak   + StringOfChar('-',80)+ sLineBreak +
            'LED - '  + FormatDateTime('dd/mm/yy hh:nn:ss:zzz', Now) +
            StringOfChar('-', 80) + sLineBreak, False);
end;

procedure TACBrSINClass.Desativar;
begin
  if (not fpAtivo) then
    Exit;

  if (fpDevice.Porta <> '') then
    fpDevice.Desativar;

  fpAtivo := False;
end;

procedure TACBrSINClass.enviarSerial(cmd: AnsiString );
begin

  try
    fpDevice.EnviaString(cmd);
    GravaLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' EnviarSerial = ' + cmd);

  except

  end;
end;


end.
