{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou, Isaque Pinheiro e              }
{							   Nilson Sergio								   }	  
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

unit ACBrLFDBloco_I_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrLFD3505, ACBrLFDBlocos, ACBrLFDBloco_I,
     ACBrTXTClass;

type

  /// BLOCO I: LANÇAMENTOS CONTÁBEIS

  { TBloco_I }

  TBloco_I = class(TACBrLFD3505)
  private
    FRegistroI001: TRegistroI001;
    FRegistroI990: TRegistroI990;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroI001;
    procedure WriteRegistroI990;

    property RegistroI001: TRegistroI001 read FRegistroI001 write FRegistroI001;
    property RegistroI990: TRegistroI990 read FRegistroI990 write FRegistroI990;
  end;

implementation

uses StrUtils;

{ TBloco_F }

constructor TBloco_I.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_I.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_I.CriaRegistros;
begin  
  FRegistroI001 := TRegistroI001.Create;
  FRegistroI990 := TRegistroI990.Create;

  FRegistroI990.QTD_LIN_I := 0;
end;

procedure TBloco_I.LiberaRegistros;
begin
  FRegistroI001.Free;
  FRegistroI990.Free;
end;

procedure TBloco_I.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TBloco_I.WriteRegistroI001;
begin
  if Assigned(FRegistroI001) then
    with FRegistroI001 do
    begin
      Add( LFill('I001') +
           LFill(Integer(IND_MOV), 0) );

      FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
    end;
end;

procedure TBloco_I.WriteRegistroI990;
begin
  if Assigned(FRegistroI990) then
    with FRegistroI990 do
    begin
      QTD_LIN_I := QTD_LIN_I + 1;

      Add( LFill('I990') +
           LFill(QTD_LIN_I, 0) );
    end;
end;

end.
