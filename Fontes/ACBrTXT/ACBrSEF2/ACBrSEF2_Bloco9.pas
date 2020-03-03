{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou                                 }
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

unit ACBrSEF2_Bloco9;

interface
  Uses SysUtils, Classes, ACBrSEF2Conversao;

type

  TRegistroSEF9001 = class(TOpenBlocos)
  private
  public
  end;

  TRegistroSEF9900 = Class
  private
    FREG_BLC: String; /// Registro que será totalizado no próximo campo
    FQTD_REG_BLC: Integer; /// Total de registros do tipo informado no campo anterior
  public
    property REG_BLC: String read FREG_BLC write FREG_BLC;
    property QTD_REG_BLC: Integer read FQTD_REG_BLC write FQTD_REG_BLC;
  end;

  TRegistroSEF9900List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEF9900;
    procedure SetItem(Index: Integer; const Value: TRegistroSEF9900);
  public
    function New: TRegistroSEF9900;
    property Items[Index: Integer]: TRegistroSEF9900 read GetItem write SetItem;
  end;

   { TRegistroSEF9990 }

  TRegistroSEF9990 = class
  private
    fQTD_LIN_9: Integer;
  public
    property QTD_LIN_9: Integer read fQTD_LIN_9 write fQTD_LIN_9;
  end;

  /// Registro 9999 - Encerramento, Controle e Assinaturas do Arquivo Digital

  { TRegistroSEF9999 }

  TRegistroSEF9999 = class
  private
    fQTD_LIN: Integer;
  public
    property QTD_LIN: Integer read fQTD_LIN write fQTD_LIN;
  end;


  type TBloco_9 = class(TACBrSEFIIEDOC)
  private
    FRegistro9001: TRegistroSEF9001;
    FRegistro9900: TRegistroSEF9900List;
    FRegistro9990: TRegistroSEF9990;
    FRegistro9999: TRegistroSEF9999;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    property Registro9900 : TRegistroSEF9900List read FRegistro9900 write FRegistro9900;
    property Registro9990: TRegistroSEF9990      read FRegistro9990 write FRegistro9990;
    property Registro9999:  TRegistroSEF9999     read FRegistro9999 write FRegistro9999;

    procedure WriteRegistro9001;
    procedure WriteRegistro9900;
    procedure WriteRegistro9990;
    procedure WriteRegistro9999;

    constructor Create;
    destructor Destroy;override;
    procedure LimpaRegistros;
  end;


implementation

{ TRegistroSEF9900List }

function TRegistroSEF9900List.GetItem(Index: Integer): TRegistroSEF9900;
begin
  Result := TRegistroSEF9900(Get(Index));
end;

function TRegistroSEF9900List.New: TRegistroSEF9900;
begin
  Result := TRegistroSEF9900.Create;
  Add(Result);
end;

procedure TRegistroSEF9900List.SetItem(Index: Integer;
  const Value: TRegistroSEF9900);
begin
  Put(Index, Value);
end;

{ TBloco9 }

constructor TBloco_9.Create;
begin
   inherited ;
   CriaRegistros;
end;

procedure TBloco_9.CriaRegistros;
begin
   FRegistro9001 := TRegistroSEF9001.Create;
   FRegistro9900 := TRegistroSEF9900List.Create;
   FRegistro9990 := TRegistroSEF9990.Create;
   FRegistro9999 := TRegistroSEF9999.Create;
end;

destructor TBloco_9.Destroy;
begin
   LiberaRegistros;
   inherited;
end;

procedure TBloco_9.LiberaRegistros;
begin
   FRegistro9001.Free;
   FRegistro9900.Free;
   FRegistro9990.Free;
   FRegistro9999.Free;
end;

procedure TBloco_9.LimpaRegistros;
begin
   /// Limpa os Registros
   LiberaRegistros;
   Conteudo.Clear;

   /// Recriar os Registros Limpos
   CriaRegistros;
end;

procedure TBloco_9.WriteRegistro9001;
begin
   if Assigned(FRegistro9001) then
   begin
      with FRegistro9001 do
      begin
         Add( LFill('9001') +
              LFill(Integer(IND_MOV), 0) );
      end;

      FRegistro9990.QTD_LIN_9 := FRegistro9990.QTD_LIN_9 + 1;
   end;
end;

procedure TBloco_9.WriteRegistro9900;
var
  intFor: Integer;
begin
    if Assigned(FRegistro9900) then
    for intFor := 0 to FRegistro9900.Count - 1 do
    begin
       with FRegistro9900.Items[intFor] do
       begin
          Add( LFill('9900') +
               LFill(REG_BLC) +
               LFill(QTD_REG_BLC, 0) );

          FRegistro9990.QTD_LIN_9 := FRegistro9990.QTD_LIN_9 + 1;
       end;
    end;
end;

procedure TBloco_9.WriteRegistro9990;
begin
   if Assigned(FRegistro9990) then
   with FRegistro9990 do
   begin
      QTD_LIN_9 := QTD_LIN_9 + 2;

      Add( LFill('9990') +
           LFill(QTD_LIN_9, 0) );
   end;
end;

procedure TBloco_9.WriteRegistro9999;
begin
   if Assigned(FRegistro9999) then
   with FRegistro9999 do
   begin
      Add( LFill('9999') +
           LFill(QTD_LIN, 0) );
   end;
end;

end.
