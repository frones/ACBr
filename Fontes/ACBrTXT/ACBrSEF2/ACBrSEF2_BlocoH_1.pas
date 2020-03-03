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

unit ACBrSEF2_BlocoH_1;

interface
  Uses SysUtils, Classes, ACBrSEF2_BlocoH, ACBrSEF2Conversao;

  type
  { TBloco_H }
  
  TBloco_H = class(TACBrSEFIIEDOC)
  private
    FRegistroH001 : TRegistroSEFH001;
    FRegistroH990 : TRegistroSEFH990;

    FRegistroH020Count: Integer;
    FRegistroH030Count: Integer;
    FRegistroH040Count: Integer;
    FRegistroH050Count: Integer;
    FRegistroH060Count: Integer;

    procedure WriteRegistroH020(RegH001: TRegistroSEFH001);
    procedure WriteRegistroH030(RegH001: TRegistroSEFH001);
    procedure WriteRegistroH040(RegH001: TRegistroSEFH001);
    procedure WriteRegistroH050(RegH001: TRegistroSEFH001);
    procedure WriteRegistroH060(RegH001: TRegistroSEFH001);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy;override;
    procedure LimpaRegistros;


    function RegistroH001New : TRegistroSEFH001;
    function RegistroH020New : TRegistroSEFH020;
    function RegistroH030New : TRegistroSEFH030;
    function RegistroH040New : TRegistroSEFH040;
    function RegistroH050New : TRegistroSEFH050;
    function RegistroH060New : TRegistroSEFH060;


    procedure WriteRegistroH001;
    procedure WriteRegistroH990;

    property RegistroH001: TRegistroSEFH001 read FRegistroH001 write FRegistroH001;
    property RegistroH990: TRegistroSEFH990 read FRegistroH990 write FRegistroH990;
    
	  property RegistroH020Count: Integer read FRegistroH020Count write FRegistroH020Count;
    property RegistroH030Count: Integer read FRegistroH030Count write FRegistroH030Count;
    property RegistroH040Count: Integer read FRegistroH040Count write FRegistroH040Count;
    property RegistroH050Count: Integer read FRegistroH050Count write FRegistroH050Count;
    property RegistroH060Count: Integer read FRegistroH060Count write FRegistroH060Count;

  end;


implementation

function IntToStrNull(AInteger : Integer) : string;
begin
   if AInteger = 0 then
     Result := ''
   else
     Result := IntToStr(AInteger);
end;


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

procedure TBloco_H.WriteRegistroH001;
var
  Astr: String;
begin
  if Assigned(FRegistroH001) then
  begin
    with FRegistroH001 do
    begin
      Astr := LFill( 'H001' )+
              LFill(Integer(IND_DAD), 1);
      Add(Astr);

      WriteRegistroH020(FRegistroH001);
      WriteRegistroH030(FRegistroH001);
      WriteRegistroH040(FRegistroH001);
      WriteRegistroH050(FRegistroH001);
      WriteRegistroH060(FRegistroH001);
    end;

      RegistroH990.QTD_LIN_H := RegistroH990.QTD_LIN_H + 1;
  end;
end;

procedure TBloco_H.WriteRegistroH020(RegH001: TRegistroSEFH001);
var
 intFor : Integer;
 RegH020 : TRegistroSEFH020;
begin
   for intFor := 0 to RegH001.RegistroH020.Count - 1 do
   begin
     RegH020 := TRegistroSEFH020(RegH001.RegistroH020.Items[intFor]);
      with RegH020 do
         begin
            Add( LFill('H020')   +
                 LFill(IntToStr(IND_DT))+
                 LFill(DT_INV)+
                 LFill(VL_ESTQ,2)+
                 LFill(VL_ICMS_REC,2)+
                 LFill(VL_IPI_REC,2)+
                 LFill(VL_PIS_REC,2)+
                 LFill(VL_COFINS_REC,2)+
                 LFill(VL_TRIB_NC,2)+
                 LFill(VL_ESTQ_NC,2)+
                 LFill(NUM_LCTO)+
                 LFill(COD_INF_OBS));
         end;
         RegistroH990.QTD_LIN_H := RegistroH990.QTD_LIN_H + 1;
      end;
      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistroH020Count := FRegistroH020Count +  RegH001.RegistroH020.Count;
end;


procedure TBloco_H.WriteRegistroH030(RegH001: TRegistroSEFH001);
var
 intFor : Integer;
 RegH030 : TRegistroSEFH030;
begin
   for intFor := 0 to RegH001.RegistroH030.Count - 1 do
   begin
     RegH030 := TRegistroSEFH030(RegH001.RegistroH030.Items[intFor]);
      with RegH030 do
         begin
            Add( LFill('H030')   +
                 LFill(IND_POSSE,1)+
                 LFill(COD_PART)+
                 LFill(IND_ITEM,1)+
                 LFill(COD_NCM,8) +
                 LFill(COD_ITEM) +
                 LFill(UNID,2)+
                 LFill(VL_UNIT,2)+
                 LFill(QTD,3)+
                 LFill(VL_ITEM,2)+
                 LFill(VL_ICMS_REC_I,2)+
                 DFill(VL_IPI_REC_I,2, true) +
                 LFill(VL_PIS_REC_I,2)+
                 LFill(VL_COFINS_REC_I,2)+
                 LFill(VL_TRIB_NC_I,2)+
                 LFill(COD_INF_OBS));
         end;
         RegistroH990.QTD_LIN_H := RegistroH990.QTD_LIN_H + 1;
      end;
      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistroH030Count := FRegistroH030Count + RegH001.RegistroH030.Count;
end;

procedure TBloco_H.WriteRegistroH040(RegH001: TRegistroSEFH001);
var
 intFor : Integer;
 RegH040 : TRegistroSEFH040;
begin
   for intFor := 0 to RegH001.RegistroH040.Count - 1 do
   begin
     RegH040 := TRegistroSEFH040(RegH001.RegistroH040.Items[intFor]);
      with RegH040 do
         begin
            Add( LFill('H040')   +
                 LFill(IND_POSSE,1)+
                 LFill(VL_SUB_POSSE,2));
         end;
         FRegistroH990.QTD_LIN_H := FRegistroH990.QTD_LIN_H + 1;
      end;
      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistroH040Count := FRegistroH040Count + RegH001.RegistroH040.Count;
end;

procedure TBloco_H.WriteRegistroH050(RegH001: TRegistroSEFH001);
var
 intFor : Integer;
 RegH050 : TRegistroSEFH050;
begin
   for intFor := 0 to RegH001.RegistroH050.Count - 1 do
   begin
     RegH050 := TRegistroSEFH050(RegH001.RegistroH050.Items[intFor]);
      with RegH050 do
         begin
            Add( LFill('H050')   +
                 LFill(IND_ITEM,1)+
                 LFill(VL_SUB_ITEM,0));
         end;
         FRegistroH990.QTD_LIN_H := FRegistroH990.QTD_LIN_H + 1;
      end;
      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistroH050Count := FRegistroH050Count + RegH001.RegistroH050.Count;
end;

procedure TBloco_H.WriteRegistroH060(RegH001: TRegistroSEFH001);
var
 intFor : Integer;
 RegH060 : TRegistroSEFH060;
begin
   for intFor := 0 to RegH001.RegistroH060.Count - 1 do
   begin
     RegH060 := TRegistroSEFH060(RegH001.RegistroH060.Items[intFor]);
      with RegH060 do
         begin
            Add( LFill('H060')   +
                 LFill(COD_NCM,8)+
                 LFill(VL_SUB_NCM,2));
         end;
         FRegistroH990.QTD_LIN_H := FRegistroH990.QTD_LIN_H + 1;
      end;
      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistroH060Count := FRegistroH060Count + RegH001.RegistroH060.Count;
 end;

procedure TBloco_H.WriteRegistroH990;
var
  strLinha : String;
begin
   //--Before
   strLinha := '';

   if Assigned(RegistroH990) then
   begin
      with RegistroH990 do
      begin
        QTD_LIN_H := QTD_LIN_H + 1;

        strLinha := LFill('H990') +
                    LFill(QTD_LIN_H,0);
        Add(strLinha);
      end;
   end;
end;

procedure TBloco_H.CriaRegistros;
begin
  FRegistroH001 := TRegistroSEFH001.Create;
  FRegistroH990 := TRegistroSEFH990.Create;

  FRegistroH020Count := 0;
  FRegistroH030Count := 0;
  FRegistroH040Count := 0;
  FRegistroH050Count := 0;
  FRegistroH060Count := 0;

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

function TBloco_H.RegistroH001New: TRegistroSEFH001;
begin
   Result := FRegistroH001;
end;

function TBloco_H.RegistroH020New : TRegistroSEFH020;
begin
  Result :=  FRegistroH001.RegistroH020.New();
end;

function TBloco_H.RegistroH030New: TRegistroSEFH030;
begin
  Result := FRegistroH001.RegistroH030.New();
end;

function TBloco_H.RegistroH040New: TRegistroSEFH040;
begin
  Result := FRegistroH001.RegistroH040.New();
end;

function TBloco_H.RegistroH050New: TRegistroSEFH050;
begin
  Result := FRegistroH001.RegistroH050.New();
end;

function TBloco_H.RegistroH060New: TRegistroSEFH060;
begin
  Result := FRegistroH001.RegistroH060.New();
end;



end.
