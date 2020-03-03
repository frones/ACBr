{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: João Pedro R Costa                              }
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

unit ACBrDeSTDABloco_9_Class;

interface

uses SysUtils, Classes, DateUtils, ACBRDeSTDA4715, ACBrDeSTDABloco_9;

type
  /// TBloco_9 -
  TBloco_9 = class(TACBrDeSTDA4715)
  private
    FRegistro9001: TRegistro9001;      /// BLOCO 9 - Registro9001
    FRegistro9020: TRegistro9020List;  /// BLOCO 9 - Lista de Registro9020
    FRegistro9030: TRegistro9030List;  /// BLOCO 9 - Lista de Registro9030
    FRegistro9900: TRegistro9900List;  /// BLOCO 9 - Lista de Registro9900
    FRegistro9990: TRegistro9990;      /// BLOCO 9 - Registro9990
    FRegistro9999: TRegistro9999;      /// BLOCO 9 - Registro9999
    ///

    FRegistro9020Count : Integer;
    FRegistro9030Count : Integer;

    procedure WriteRegistro9020(Reg9001: TRegistro9001);
    procedure WriteRegistro9030(Reg9020: TRegistro9020);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    function Registro9020New: TRegistro9020;
    function Registro9030New: TRegistro9030;

    procedure WriteRegistro9001;
    procedure WriteRegistro9900;
    procedure WriteRegistro9990;
    procedure WriteRegistro9999;

    property Registro9020Count: Integer           read FRegistro9020Count write FRegistro9020Count;
    property Registro9030Count: Integer           read FRegistro9030Count write FRegistro9030Count;
    property Registro9001     : TRegistro9001     read FRegistro9001      write FRegistro9001;
    property Registro9900     : TRegistro9900List read FRegistro9900      write FRegistro9900;
    property Registro9990     : TRegistro9990     read FRegistro9990      write FRegistro9990;
    property Registro9999     : TRegistro9999     read FRegistro9999      write FRegistro9999;
  end;

implementation

uses
  ACBrDeSTDABlocos;

constructor TBloco_9.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_9.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_9.CriaRegistros;
begin
  FRegistro9001 := TRegistro9001.Create;
  FRegistro9020 := TRegistro9020List.Create;
  FRegistro9030 := TRegistro9030List.Create;
  FRegistro9900 := TRegistro9900List.Create;
  FRegistro9990 := TRegistro9990.Create;
  FRegistro9999 := TRegistro9999.Create;

  FRegistro9020Count := 0;
  FRegistro9030Count := 0;
  ///
  FRegistro9990.QTD_LIN_9 := 0;
  FRegistro9999.QTD_LIN   := 0;
end;

procedure TBloco_9.LiberaRegistros;
begin
  FRegistro9001.Free;
  FRegistro9020.Free;
  FRegistro9030.Free;
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

function TBloco_9.Registro9020New: TRegistro9020;
//var
//  G620Count: integer;
begin
//   G620Count := FRegistroG001.RegistroG020.RegistroG620.Count -1;
   //
   Result := FRegistro9001.Registro9020.New;
end;

function TBloco_9.Registro9030New: TRegistro9030;
var
  count9020: integer;
begin
   count9020 := FRegistro9001.Registro9020.Count - 1;
   //
   Result := FRegistro9001.Registro9020.Items[count9020].Registro9030.New;
end;

procedure TBloco_9.WriteRegistro9001;
begin
  if Assigned(Registro9001) then
  begin
     with Registro9001 do
     begin
       Add( LFill( '9001' ) +
            LFill( Integer(IND_MOV), 0) ) ;
       ///
       Registro9990.QTD_LIN_9 := Registro9990.QTD_LIN_9 + 1;
     end;
     WriteRegistro9020( Registro9001 ) ;
  end;
end;

procedure TBloco_9.WriteRegistro9020(Reg9001: TRegistro9001);
var
  intFor: integer;
  strLEIAUTE: string;
begin
  if Assigned(Reg9001.Registro9020) then
  begin
    for intFor := 0 to Reg9001.Registro9020.Count - 1 do
    begin
      with Reg9001.Registro9020.Items[intFor] do
      begin
        case LEIAUTE of
          laLFPD: strLEIAUTE := 'LFPD';
          laLECD: strLEIAUTE := 'LECD';
        end;

        Add( LFill( '9020') +
             LFill( strLEIAUTE ) +
             LFill( ARQ_DT_INI, 'ddmmyyyy' ) +
             LFill( ARQ_DT_FIN, 'ddmmyyyy' ) +
             LFill( ARQ_NOME_EMPR ) +
             LFill( ARQ_CNPJ ) +
             LFill( ARQ_UF ) +
             LFill( ARQ_IE ) +
             LFill( ARQ_COD_MUN, 7 ) +
             LFill( ARQ_IM ) +
             LFill( VAZIO ) +
             LFill( ARQ_SUFRAMA ) +
             LFill( ARQ_COD_VER ) +
             LFill( ARQ_COD_FIN ) +
             LFill( ARQ_COD_CTD ) +
             LFill( ARQ_PAIS ) +
             LFill( ARQ_CPF ) +
             LFill( ARQ_FANTASIA ) +
             LFill( ARQ_QTD_LIN, 0 ) +
             LFill( ARQ_NOME ) +
             LFill( ASS_HASH) ) ;
      end;
      WriteRegistro9030( Reg9001.Registro9020.Items[intFor] ) ;

      Registro9990.QTD_LIN_9 := Registro9990.QTD_LIN_9 + 1;
    end;
    FRegistro9020Count := FRegistro9020Count + Reg9001.Registro9020.Count;
  end;
end;

procedure TBloco_9.WriteRegistro9030(Reg9020: TRegistro9020);
var
  intFor: integer;
begin
  if Assigned(Reg9020.Registro9030) then
  begin
    for intFor := 0 to Reg9020.Registro9030.Count - 1 do
    begin
      with Reg9020.Registro9030.Items[intFor] do
      begin

        Add( LFill( '9030') +
             LFill( ARQ_LIN_BLC ) +
             LFill( Integer(ARQ_QTD_LIN_BLC), 0) ) ;
      end;
      Registro9990.QTD_LIN_9 := Registro9990.QTD_LIN_9 + 1;
    end;
    FRegistro9030Count := FRegistro9030Count + Reg9020.Registro9030.Count;
  end;
end;

procedure TBloco_9.WriteRegistro9900;
var
  intFor: integer;
begin
  if Assigned(Registro9900) then
  begin
     for intFor := 0 to Registro9900.Count - 1 do
     begin
        with Registro9900.Items[intFor] do
        begin
           Add( LFill('9900') +
                LFill(REG_BLC) +
                LFill(QTD_REG_BLC,0) ) ;
        end;
     end;
     Registro9990.QTD_LIN_9 := Registro9990.QTD_LIN_9 + Registro9900.Count + 2;
  end;
end;

procedure TBloco_9.WriteRegistro9990;
begin
  if Assigned(Registro9990) then
  begin
     with Registro9990 do
     begin
        Add( LFill('9990') +
             LFill(QTD_LIN_9,0) ) ;
     end;
  end;
end;

procedure TBloco_9.WriteRegistro9999;
begin
  if Assigned(Registro9999) then
  begin
     with Registro9999 do
     begin
        Add( LFill('9999') +
             LFill(QTD_LIN,0) ) ;
     end;
  end;
end;

end.
