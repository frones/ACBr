{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					                    2015   Isaque Pinheiro	    	             }
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
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_E_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_E, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_E -

  { TBloco_E }

  TBloco_E = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroE001: TRegistroE001;
    FRegistroE990: TRegistroE990;

    procedure CriaRegistros;
    procedure LiberaRegistros;

  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    constructor Create;
    destructor Destroy;

    procedure LimpaRegistros;
    procedure WriteRegistroE001;
    procedure WriteRegistroE990;

    property RegistroE001: TRegistroE001 read FRegistroE001 write FRegistroE001;
    property RegistroE990: TRegistroE990 read FRegistroE990 write FRegistroE990;
  published
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_E }

constructor TBloco_E.Create;
begin
  CriaRegistros;
end;

procedure TBloco_E.CriaRegistros;
begin
  FRegistroE001 := TRegistroE001.Create;
  FRegistroE990 := TRegistroE990.Create;

  FRegistroE990.QTD_LIN := 0;
end;

destructor TBloco_E.Destroy;
begin
  LiberaRegistros;
end;

procedure TBloco_E.LiberaRegistros;
begin
  FRegistroE001.Free;
  FRegistroE990.Free;
end;

procedure TBloco_E.LimpaRegistros;
begin
  LiberaRegistros;
  Conteudo.Clear;

  CriaRegistros;
end;

procedure TBloco_E.WriteRegistroE001;
begin
  if Assigned(FRegistroE001) then begin
    with FRegistroE001 do begin
      Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(E-E001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
      Add(LFill('E001') +
          LFill( Integer(IND_DAD), 1));
      FRegistroE990.QTD_LIN:= FRegistroE990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_E.WriteRegistroE990;
begin
  if Assigned(FRegistroE990) then begin
    with FRegistroE990 do begin
      QTD_LIN := QTD_LIN + 1;
      Add(LFill('E990') +
          LFill(QTD_LIN, 0));
    end;
  end;
end;

end.
