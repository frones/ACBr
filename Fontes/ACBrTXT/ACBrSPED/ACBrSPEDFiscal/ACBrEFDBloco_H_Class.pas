{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                       Isaque Pinheiro                        }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrEFDBloco_H_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEFDBloco_H, StrUtils,
     ACBrEFDBloco_0_Class, ACBrEFDBlocos;

type
  /// TBLOCO_H -
  TBloco_H = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;

    FRegistroH001: TRegistroH001;      /// BLOCO H - RegistroH001
    FRegistroH990: TRegistroH990;      /// BLOCO H - RegistroH990

    FRegistroH005Count: Integer;
    FRegistroH010Count: Integer;
    FRegistroH020Count: Integer;
    FRegistroH030Count: Integer;


    procedure WriteRegistroH005(RegH001: TRegistroH001);
    procedure WriteRegistroH010(RegH005: TRegistroH005);
    procedure WriteRegistroH020(RegH010: TRegistroH010);
    procedure WriteRegistroH030(RegH010: TRegistroH010);


    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function RegistroH001New: TRegistroH001;
    function RegistroH005New: TRegistroH005;
    function RegistroH010New: TRegistroH010;
    function RegistroH020New: TRegistroH020;
    function RegistroH030New: TRegistroH030;


    procedure WriteRegistroH001;
    procedure WriteRegistroH990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroH001: TRegistroH001 read FRegistroH001 write FRegistroH001;
    property RegistroH990: TRegistroH990 read FRegistroH990 write FRegistroH990;

    property RegistroH005Count: Integer read FRegistroH005Count write FRegistroH005Count;
    property RegistroH010Count: Integer read FRegistroH010Count write FRegistroH010Count;
    property RegistroH020Count: Integer read FRegistroH020Count write FRegistroH020Count;
    property RegistroH030Count: Integer read FRegistroH030Count write FRegistroH030Count;
  end;

implementation

{ TBloco_H }

constructor TBloco_H.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_H.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_H.CriaRegistros;
begin
  FRegistroH001 := TRegistroH001.Create;
  FRegistroH990 := TRegistroH990.Create;

  FRegistroH005Count := 0;
  FRegistroH010Count := 0;
  FRegistroH020Count := 0;
  FRegistroH030Count := 0;


  FRegistroH990.QTD_LIN_H := 0;
end;

procedure TBloco_H.LiberaRegistros;
begin
  FRegistroH001.Free;
  FRegistroH990.Free;
end;

procedure TBloco_H.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_H.RegistroH001New: TRegistroH001;
begin
   Result := FRegistroH001;
end;

function TBloco_H.RegistroH005New: TRegistroH005;
begin
   Result := FRegistroH001.RegistroH005.New();
end;

function TBloco_H.RegistroH010New: TRegistroH010;
var
H005: TRegistroH005;
H005Count: Integer;
begin
   H005Count := FRegistroH001.RegistroH005.Count -1;
   if H005Count = -1 then
      raise Exception.Create('O registro H010 deve ser filho do registro H005, e não existe nenhum H005 pai!');

   H005   := FRegistroH001.RegistroH005.Items[H005Count];
   Result := H005.RegistroH010.New();
end;

function TBloco_H.RegistroH020New: TRegistroH020;
var
H010: TRegistroH010;
H005Count: integer;
H010Count: integer;
begin
   H005Count := FRegistroH001.RegistroH005.Count -1;
   H010Count := FRegistroH001.RegistroH005.Items[H005Count].RegistroH010.Count -1;
   if H010Count = -1 then
      raise Exception.Create('O registro H020 deve ser filho do registro H010, e não existe nenhum H010 pai!');

   H010   := FRegistroH001.RegistroH005.Items[H005Count].RegistroH010.Items[H010Count];
   Result := H010.RegistroH020.New(H010);
end;

function TBloco_H.RegistroH030New: TRegistroH030;
var
H010: TRegistroH010;
H005Count: integer;
H010Count: integer;
begin
   H005Count := FRegistroH001.RegistroH005.Count -1;
   H010Count := FRegistroH001.RegistroH005.Items[H005Count].RegistroH010.Count -1;
   if H010Count = -1 then
      raise Exception.Create('O registro H030 deve ser filho do registro H010, e não existe nenhum H010 pai!');

   H010   := FRegistroH001.RegistroH005.Items[H005Count].RegistroH010.Items[H010Count];
   Result := H010.RegistroH030.New(H010);
end;


procedure TBloco_H.WriteRegistroH001;
begin
  if Assigned(RegistroH001) then
  begin
     with RegistroH001 do
     begin
       Add( LFill( 'H001' ) +
            LFill( Integer(IND_MOV), 0 ) ) ;

       if IND_MOV = imComDados then
       begin
          WriteRegistroH005(FRegistroH001);
       end;
     end;

     RegistroH990.QTD_LIN_H := RegistroH990.QTD_LIN_H + 1;
  end;
end;

procedure TBloco_H.WriteRegistroH005(RegH001: TRegistroH001);
var
  intFor: integer;
  strMotInv: string;
begin
  if Assigned( RegH001.RegistroH005 ) then
  begin
     for intFor := 0 to RegH001.RegistroH005.Count - 1 do
     begin
        with RegH001.RegistroH005.Items[intFor] do
        begin
          if (FBloco_0.Registro0000.COD_VER  >= vlVersao104) then
          begin
              if DT_FIN >= EncodeDate(2012,07,01) then
              begin
                case MOT_INV of
                  miFinalPeriodo:       strMotInv := '01';
                  miMudancaTributacao:  strMotInv := '02';
                  miBaixaCadastral:     strMotInv := '03';
                  miRegimePagamento:    strMotInv := '04';
                  miDeterminacaoFiscos: strMotInv := '05';
                else
                  strMotInv := '01';
                end;

                Add( LFill('H005') +
                     LFill( DT_INV ) +
                     LFill( VL_INV, 0) +
                     LFill( strMotInv ) ) ;
              end
              else
              begin
                Add( LFill('H005') +
                     LFill( DT_INV ) +
                     LFill( VL_INV, 0) ) ;
              end;
          end
          else //versões vlVersao103 para trás.
          begin
            Add( LFill('H005') +
                 LFill( DT_INV ) +
                 LFill( VL_INV, 0) ) ;
          end;
        end;
        /// Registros FILHOS
        WriteRegistroH010( RegH001.RegistroH005.Items[intFor] );

        RegistroH990.QTD_LIN_H := RegistroH990.QTD_LIN_H + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroH005Count := FRegistroH005Count + RegH001.RegistroH005.Count;
  end;
end;

procedure TBloco_H.WriteRegistroH010(RegH005: TRegistroH005);
var
  intFor: integer;
begin
  if Assigned( RegH005.RegistroH010 ) then
  begin
     for intFor := 0 to RegH005.RegistroH010.Count - 1 do
     begin
        with RegH005.RegistroH010.Items[intFor] do
        begin
          Add( LFill('H010') +
               LFill( COD_ITEM ) +
               LFill( UNID ) +
               DFill( QTD, 3 ) +
               DFill( VL_UNIT, 6 ) +
               LFill( VL_ITEM, 0, 2 ) +
               LFill( Integer(IND_PROP), 0 ) +
               LFill( COD_PART ) +
               LFill( TXT_COMPL ) +
               LFill( COD_CTA ) +
               IfThen( DT_INI >= EncodeDate(2015,01,01), LFill(VL_ITEM_IR, 0, 2), EmptyStr )) ;/// campo somente inserido a partir de janeiro 2015
        end;

        /// Registros FILHOS
        WriteRegistroH020( RegH005.RegistroH010.Items[intFor] );
        WriteRegistroH030( RegH005.RegistroH010.Items[intFor] );

        RegistroH990.QTD_LIN_H := RegistroH990.QTD_LIN_H + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroH010Count := FRegistroH010Count + RegH005.RegistroH010.Count;
  end;
end;

procedure TBloco_H.WriteRegistroH020(RegH010: TRegistroH010);
var
  intFor: integer;
begin
  if FBloco_0.Registro0000.COD_VER >= vlVersao104 then
    if DT_INI >= EncodeDate(2012,07,01) then
    begin
      if Assigned( RegH010.RegistroH020 ) then
      begin
         for intFor := 0 to RegH010.RegistroH020.Count - 1 do
         begin
            with RegH010.RegistroH020.Items[intFor] do
            begin
              Add( LFill('H020') +
                   LFill( CST_ICMS ) +
                   LFill( BC_ICMS, 0, 2 ) +
                   LFill( VL_ICMS, 0, 2 ) ) ;
            end;

            RegistroH990.QTD_LIN_H := RegistroH990.QTD_LIN_H + 1;
         end;
         /// Variavél para armazenar a quantidade de registro do tipo.
         FRegistroH020Count := FRegistroH020Count + RegH010.RegistroH020.Count;
      end;
    end;
end;


procedure TBloco_H.WriteRegistroH030(RegH010: TRegistroH010);
var
  intFor: integer;
begin
  if FBloco_0.Registro0000.COD_VER >= vlVersao104 then
    if DT_INI >= EncodeDate(2012,07,01) then
    begin
      if Assigned( RegH010.RegistroH030 ) then
      begin
         for intFor := 0 to RegH010.RegistroH030.Count - 1 do
         begin
            with RegH010.RegistroH030.Items[intFor] do
            begin
              Add( LFill('H030') +
                   LFill( VL_ICMS_OP, 0, 6 ) +
                   LFill( VL_BC_ICMS_ST, 0, 6 ) +
                   LFill( VL_ICMS_ST, 0, 6 ) +
                   LFill( VL_FCP, 0, 6 ) ) ;
            end;

            RegistroH990.QTD_LIN_H := RegistroH990.QTD_LIN_H + 1;
         end;
         /// Variavél para armazenar a quantidade de registro do tipo.
         FRegistroH030Count := FRegistroH030Count + RegH010.RegistroH030.Count;
      end;
    end;
end;

procedure TBloco_H.WriteRegistroH990;
begin
  if Assigned(RegistroH990) then
  begin
     with RegistroH990 do
     begin
       QTD_LIN_H := QTD_LIN_H + 1;
       ///
       Add( LFill('H990') +
            LFill(QTD_LIN_H,0) ) ;
     end;
  end;
end;

end.
