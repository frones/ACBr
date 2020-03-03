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

unit ACBrDeSTDABloco_G_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrDeSTDABlocos,
     ACBRDeSTDA4715, ACBrDeSTDABloco_G;

type

  /// Bloco A - DOCUMENTOS FISCAIS DO ISS

  { TBloco_G }

  TBloco_G = class(TACBrDeSTDA4715)
  private
    FRegistroG001: TRegistroG001;      /// BLOCO D - RegistroD001
    FRegistroG990: TRegistroG990;      /// BLOCO D - RegistroD990

    FRegistroG605Count: integer;
    FRegistroG610Count: integer;
    FRegistroG615Count: integer;
    FRegistroG620Count: integer;
    FRegistroG625Count: integer;

    procedure WriteRegistroG020(RegG001: TRegistroG001) ;
    procedure WriteRegistroG600(RegG020: TRegistroG020) ;
    procedure WriteRegistroG605(RegG600: TRegistroG600) ;
    procedure WriteRegistroG610(RegG020: TRegistroG020) ;
    procedure WriteRegistroG615(RegG610: TRegistroG610) ;
    procedure WriteRegistroG620(RegG020: TRegistroG020) ;
    procedure WriteRegistroG625(RegG620: TRegistroG620) ;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    function RegistroG001New: TRegistroG001;
    function RegistroG020New: TRegistroG020;
    function RegistroG600New: TRegistroG600;
    function RegistroG605New: TRegistroG605;
    function RegistroG610New: TRegistroG610;
    function RegistroG615New: TRegistroG615;
    function RegistroG620New: TRegistroG620;
    function RegistroG625New: TRegistroG625;

    procedure WriteRegistroG001;
    procedure WriteRegistroG990;

    property RegistroG001     : TRegistroG001 read FRegistroG001      write FRegistroG001;
    property RegistroG990     : TRegistroG990 read FRegistroG990      write FRegistroG990;
    property RegistroG605Count: integer       read FRegistroG605Count write FRegistroG605Count;
    property RegistroG610Count: integer       read FRegistroG610Count write FRegistroG610Count;
    property RegistroG615Count: integer       read FRegistroG615Count write FRegistroG615Count;
    property RegistroG620Count: integer       read FRegistroG620Count write FRegistroG620Count;
    property RegistroG625Count: integer       read FRegistroG625Count write FRegistroG625Count;
  end;

implementation

{ TBloco_G }

constructor TBloco_G.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TBloco_G.CriaRegistros;
begin
  FRegistroG001 := TRegistroG001.Create;
  FRegistroG990 := TRegistroG990.Create;

  FRegistroG605Count := 0;
  FRegistroG610Count := 0;
  FRegistroG615Count := 0;
  FRegistroG620Count := 0;
  FRegistroG625Count := 0;

  FRegistroG990.QTD_LIN_G := 0;
end;

destructor TBloco_G.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_G.LiberaRegistros;
begin
  FRegistroG001.Free;
  FRegistroG990.Free;
end;

procedure TBloco_G.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_G.RegistroG001New: TRegistroG001;
begin
   Result := FRegistroG001;
end;

function TBloco_G.RegistroG020New: TRegistroG020;
begin
   Result := FRegistroG001.RegistroG020;
end;

function TBloco_G.RegistroG600New: TRegistroG600;
begin
   Result := FRegistroG001.RegistroG020.RegistroG600;
end;

function TBloco_G.RegistroG605New: TRegistroG605;
begin
   Result := FRegistroG001.RegistroG020.RegistroG600.RegistroG605.New;
end;

function TBloco_G.RegistroG610New: TRegistroG610;
begin
  Result := FRegistroG001.RegistroG020.RegistroG610;
end;

function TBloco_G.RegistroG615New: TRegistroG615;
begin
   Result := FRegistroG001.RegistroG020.RegistroG610.RegistroG615.New;
end;

function TBloco_G.RegistroG620New: TRegistroG620;
begin
   Result := FRegistroG001.RegistroG020.RegistroG620.New;
end;

function TBloco_G.RegistroG625New: TRegistroG625;
var
  G620Count: integer;
begin
   G620Count := FRegistroG001.RegistroG020.RegistroG620.Count -1;
   //
   Result := FRegistroG001.RegistroG020.RegistroG620.Items[G620Count].RegistroG625.New;
end;

procedure TBloco_G.WriteRegistroG001;
begin
  if Assigned(FRegistroG001) then
  begin
     with FRegistroG001 do
     begin
       Add( LFill( 'G001' ) +
            LFill( Integer(IND_MOV), 0 ) ) ;

       if IND_MOV = imComDados then
       begin
         WriteRegistroG020 ( FRegistroG001 ) ;
       end;
     end;

     RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
  end;
end;

procedure TBloco_G.WriteRegistroG020(RegG001: TRegistroG001);
begin
  if Assigned(RegG001.RegistroG020) then
  begin
    with RegG001.RegistroG020 do
    begin
      Add( LFill('G020') +
           LFill( Integer (IND_GEF),0 ) +
           LFill( DT_INI ) +
           LFill( DT_FIN ) ) ;
      ///
      RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
    end;
    /// Registros FILHOS
    WriteRegistroG600( RegG001.RegistroG020 );
    WriteRegistroG610( RegG001.RegistroG020 );
    WriteRegistroG620( RegG001.RegistroG020 );
  end;
end;

procedure TBloco_G.WriteRegistroG600(RegG020: TRegistroG020);
begin
  if Assigned(RegG020.RegistroG600) then
  begin
    with RegG020.RegistroG600 do
    begin

      Add( LFill('G600') +
           VDFill( VL_TOT_NF, 2) +
           VDFill( VL_TOT_AJ, 2) +
           VDFill( VL_TOT_DA, 2) ) ;
      ///
      RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
    end;
    /// Registros FILHOS
    WriteRegistroG605( RegG020.RegistroG600 ) ;
  end;
end;

procedure TBloco_G.WriteRegistroG605(RegG600: TRegistroG600);
var
  intFor: Integer;
begin
  if Assigned(RegG600.RegistroG605) then
  begin
    for intFor := 0 to RegG600.RegistroG605.Count - 1 do
    begin
      with RegG600.RegistroG605.Items[intFor] do
      begin
        Add( LFill('G605') +
             LFill( Integer (IND_SIT),0 ) +
             VDFill( VL_TOT_ANTC_NF, 2) +
             VDFill( VL_TOT_AJ_ANTC, 2) +
             VDFill( VL_TOT_DA_ANTC, 2) ) ;
        ///
        RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
      end;
    end;
    FRegistroG605Count := FRegistroG605Count + RegG600.RegistroG605.Count;
  end;
end;

procedure TBloco_G.WriteRegistroG610(RegG020: TRegistroG020);
//var
//  intFor: Integer;
begin
  if Assigned(RegG020.RegistroG610) then
  begin
//    for intFor := 0 to RegG020.RegistroG610.Count - 1 do
//    begin
      with RegG020.RegistroG610 do
      begin

        Add( LFill('G610') +
             VDFill( VL_TOT_ST_NF, 2) +
             VDFill( VL_TOT_AJ_ST, 2) +
             VDFill( VL_TOT_ST_DEC, 2) ) ;
        ///
        RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
      end;
      /// Registros FILHOS
      WriteRegistroG615( RegG020.RegistroG610) ;
      FRegistroG610Count := FRegistroG610Count + 1;
//    end;
  end;
end;

procedure TBloco_G.WriteRegistroG615(RegG610: TRegistroG610);
var
  intFor: Integer;
begin
  if Assigned(RegG610.RegistroG615) then
  begin
    for intFor := 0 to RegG610.RegistroG615.Count - 1 do
    begin
      with RegG610.RegistroG615.Items[intFor] do
      begin

        Add( LFill('G615') +
             LFill(UF) +
             VDFill( VL_TOT_ST_UF_NF, 2) +
             VDFill( VL_TOT_AJ_ST_UF, 2) +
             VDFill( VL_TOT_ST_UF_DEC, 2) ) ;
        ///
        RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
      end;
    end;
    FRegistroG615Count := FRegistroG615Count + RegG610.RegistroG615.Count;
  end;
end;

procedure TBloco_G.WriteRegistroG620(RegG020: TRegistroG020);
var
  intFor: Integer;
begin
  if Assigned(RegG020.RegistroG620) then
  begin
    for intFor := 0 to RegG020.RegistroG620.Count - 1 do
    begin
      with RegG020.RegistroG620.Items[intFor] do
      begin
        Add( LFill('G620') +
             LFill(Integer (IND_OPER),0 ) +
             LFill(Integer (IND_EMIT),0 ) +
             VDFill( VL_TOT_ST_NF, 2) +
             VDFill( VL_TOT_AJ_ST, 2) +
             VDFill( VL_TOT_ST_DEC, 2) +
             VDFill( VL_TOT_ST_COMB, 2) ) ;
        ///
        RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
      end;
      /// Registros FILHOS
      WriteRegistroG625( RegG020.RegistroG620.Items[intFor] ) ;
    end;
    FRegistroG620Count := FRegistroG620Count + RegG020.RegistroG620.Count;
  end;
end;

procedure TBloco_G.WriteRegistroG625(RegG620: TRegistroG620);
var
  intFor: Integer;
begin
  if Assigned(RegG620.RegistroG625) then
  begin
    for intFor := 0 to RegG620.RegistroG625.Count - 1 do
    begin
      with RegG620.RegistroG625.Items[intFor] do
      begin
        Add( LFill('G625') +
             LFill( UF ) +
             LFill(Integer (IND_TP_ST),0 ) +
             VDFill( VL_TOT_ST_NF, 2) +
             VDFill( VL_TOT_AJ_ST, 2) +
             VDFill( VL_TOT_DEC_ST, 2) ) ;
        ///
        RegistroG990.QTD_LIN_G := RegistroG990.QTD_LIN_G + 1;
      end;
    end;
    FRegistroG625Count := FRegistroG625Count + RegG620.RegistroG625.Count;
  end;
end;

procedure TBloco_G.WriteRegistroG990;
begin
  if Assigned(RegistroG990) then
  begin
     with RegistroG990 do
     begin
       QTD_LIN_G := QTD_LIN_G + 1;
       ///
       Add( LFill('G990') +
            LFill(QTD_LIN_G,0) ) ;
     end;
  end;
end;

end.
