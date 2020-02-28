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

unit ACBrLFDBloco_J_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrLFD3505, ACBrLFDBlocos, ACBrLFDBloco_J,
     ACBrTXTClass;

type

  /// BLOCO J: INFORMAÇÕES ECONÔMICO-FISCAIS

  { TBloco_J }

  TBloco_J = class(TACBrLFD3505)
  private
    FRegistroJ001: TRegistroJ001;
    FRegistroJ990: TRegistroJ990;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroJ001;
    procedure WriteRegistroJ990;

    property RegistroJ001: TRegistroJ001 read FRegistroJ001 write FRegistroJ001;
    property RegistroJ990: TRegistroJ990 read FRegistroJ990 write FRegistroJ990;
  end;

implementation

uses StrUtils;

{ TBloco_J }

constructor TBloco_J.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_J.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_J.CriaRegistros;
begin
  FRegistroJ001 := TRegistroJ001.Create;
  FRegistroJ990 := TRegistroJ990.Create;

  FRegistroJ990.QTD_LIN_J := 0;
end;

procedure TBloco_J.LiberaRegistros;
begin
  FRegistroJ001.Free;
  FRegistroJ990.Free;
end;

procedure TBloco_J.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TBloco_J.WriteRegistroJ001;
begin
  if Assigned(FRegistroJ001) then
    with FRegistroJ001 do
    begin
      Add( LFill('J001') +
           LFill(Integer(IND_MOV), 0) );

      FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
    end;
end;

procedure TBloco_J.WriteRegistroJ990;
begin
  if Assigned(FRegistroJ990) then
    with FRegistroJ990 do
    begin
      QTD_LIN_J := QTD_LIN_J + 1;

      Add( LFill('J990') +
           LFill(QTD_LIN_J, 0) );
    end;
end;

end.
