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

unit ACBrLFDBloco_Z_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrLFD3505, ACBrLFDBlocos, ACBrLFDBloco_Z,
     ACBrTXTClass;

type

  /// BLOCO Z: REGISTROS COMPLEMENTARES

  { TBloco_Z }

  TBloco_Z = class(TACBrLFD3505)
  private
    FRegistroZ001: TRegistroZ001;
    FRegistroZ990: TRegistroZ990;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroZ001;
    procedure WriteRegistroZ990;

    property RegistroZ001: TRegistroZ001 read FRegistroZ001 write FRegistroZ001;
    property RegistroZ990: TRegistroZ990 read FRegistroZ990 write FRegistroZ990;
  end;

implementation

uses StrUtils;

{ TBloco_Z }

constructor TBloco_Z.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_Z.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_Z.CriaRegistros;
begin  
  FRegistroZ001 := TRegistroZ001.Create;
  FRegistroZ990 := TRegistroZ990.Create;

  FRegistroZ990.QTD_LIN_Z := 0;
end;

procedure TBloco_Z.LiberaRegistros;
begin
  FRegistroZ001.Free;
  FRegistroZ990.Free;
end;

procedure TBloco_Z.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TBloco_Z.WriteRegistroZ001;
begin
  if Assigned(FRegistroZ001) then
    with FRegistroZ001 do
    begin
      Add( LFill('Z001') +
           LFill(Integer(IND_MOV), 0) );

      FRegistroZ990.QTD_LIN_Z := FRegistroZ990.QTD_LIN_Z + 1;
    end;
end;

procedure TBloco_Z.WriteRegistroZ990;
begin
  if Assigned(FRegistroZ990) then
    with FRegistroZ990 do
    begin
      QTD_LIN_Z := QTD_LIN_Z + 1;

      Add( LFill('Z990') +
           LFill(QTD_LIN_Z, 0) );
    end;
end;

end.
