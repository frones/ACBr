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

unit ACBrLFDBloco_K_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrLFD3505, ACBrLFDBlocos, ACBrLFDBloco_K,
     ACBrTXTClass;

type

  /// BLOCO K: FOLHA DE PAGAMENTO

  { TBloco_K }

  TBloco_K = class(TACBrLFD3505)
  private
    FRegistroK001: TRegistroK001;
    FRegistroK990: TRegistroK990;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroK001;
    procedure WriteRegistroK990;

    property RegistroK001: TRegistroK001 read FRegistroK001 write FRegistroK001;
    property RegistroK990: TRegistroK990 read FRegistroK990 write FRegistroK990;
  end;

implementation

uses StrUtils;

{ TBloco_K }

constructor TBloco_K.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_K.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_K.CriaRegistros;
begin
  FRegistroK001 := TRegistroK001.Create;
  FRegistroK990 := TRegistroK990.Create;

  FRegistroK990.QTD_LIN_K:= 0;
end;

procedure TBloco_K.LiberaRegistros;
begin
  FRegistroK001.Free;
  FRegistroK990.Free;
end;

procedure TBloco_K.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TBloco_K.WriteRegistroK001;
begin
  if Assigned(FRegistroK001) then
    with FRegistroK001 do
    begin
      Add( LFill('K001') +
           LFill(Integer(IND_MOV), 0) );

      FRegistroK990.QTD_LIN_K := FRegistroK990.QTD_LIN_K + 1;
    end;
end;

procedure TBloco_K.WriteRegistroK990;
begin
  if Assigned(FRegistroK990) then
    with FRegistroK990 do
    begin
      QTD_LIN_K := QTD_LIN_K + 1;

      Add( LFill('K990') +
           LFill(QTD_LIN_K, 0) );
    end;
end;

end.
