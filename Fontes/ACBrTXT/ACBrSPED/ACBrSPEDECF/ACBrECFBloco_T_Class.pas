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

unit ACBrECFBloco_T_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_T, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_T -

  { TBloco_T }

  TBloco_T = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroT001: TRegistroT001;
    FRegistroT990: TRegistroT990;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    constructor Create;
    destructor Destroy;

    procedure LimpaRegistros;

    procedure WriteRegistroT001;
    procedure WriteRegistroT030;
    procedure WriteRegistroT990;

    property RegistroT001: TRegistroT001 read FRegistroT001 write FRegistroT001;
    property RegistroT990: TRegistroT990 read FRegistroT990 write FRegistroT990;
  published
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_T }

constructor TBloco_T.Create;
begin
  FRegistroT001 := TRegistroT001.Create;
  FRegistroT990 := TRegistroT990.Create;
end;

procedure TBloco_T.CriaRegistros;
begin
  FRegistroT001 := TRegistroT001.Create;
  FRegistroT990 := TRegistroT990.Create;
end;

destructor TBloco_T.Destroy;
begin
  FRegistroT001.Free;
  FRegistroT990.Free;
end;

procedure TBloco_T.LiberaRegistros;
begin
  FRegistroT001.Free;
  FRegistroT990.Free;
end;

procedure TBloco_T.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TBloco_T.WriteRegistroT001;
begin
  if Assigned(FRegistroT001) then begin
     with FRegistroT001 do begin
       Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(T-T001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Add(LFill('T001') +
           LFill( Integer(IND_DAD), 1));
       ///
       FRegistroT990.QTD_LIN:= FRegistroT990.QTD_LIN + 1;
       WriteRegistroT030;
     end;
  end;
end;

procedure TBloco_T.WriteRegistroT030;
begin
  //
end;

procedure TBloco_T.WriteRegistroT990;
begin
  if Assigned(FRegistroT990) then begin
     with FRegistroT990 do begin
       QTD_LIN := QTD_LIN + 1;
       ///
       Add(LFill('T990') +
           LFill(QTD_LIN, 0));
     end;
  end;
end;

end.
