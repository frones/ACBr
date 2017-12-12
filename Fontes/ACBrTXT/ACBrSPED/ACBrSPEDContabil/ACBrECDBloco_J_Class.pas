{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
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
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
|* 06/05/2014: Francinaldo A. da Costa
|*  - Modificações para o layout 2
|* 04/03/2015: Flavio Rubens Massaro Jr.
|* - Modificação para contemplar layout 3 referente ao ano calendario 2014
*******************************************************************************}

unit ACBrECDBloco_J_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrECDBloco_J, ACBrECDBloco_0_Class;

type
  /// TBloco_J -
  TBloco_J = class(TACBrSPED)
  private
    FRegistroJ001: TRegistroJ001;      /// BLOCO J - RegistroJ001
    FRegistroJ005: TRegistroJ005List;  /// BLOCO J - Lista de RegistroJ005
    FRegistroJ800: TRegistroJ800List;  /// BLOCO J - Lista de RegistroJ800
    FRegistroJ801: TRegistroJ801List;  /// BLOCO J - Lista de RegistroJ801

    FRegistroJ900: TRegistroJ900;      /// BLOCO J - RegistroJ900
    FRegistroJ930: TRegistroJ930List;  /// BLOCO J - Lista de RegistroJ930
    FRegistroJ935: TRegistroJ935List;  /// BLOCO J - Lista de RegistroJ935
    FRegistroJ990: TRegistroJ990;      /// BLOCO J - FRegistroJ990

    FRegistroJ100Count: Integer;
    FRegistroJ150Count: Integer;
    FRegistroJ200Count: Integer;
    FRegistroJ210Count: Integer;
    FRegistroJ215Count: Integer;

    FBloco_0: TBloco_0;
    FRegistroJ800Count: Integer;
    FRegistroJ801Count: Integer;

    procedure WriteRegistroJ100(RegJ005: TRegistroJ005);
    procedure WriteRegistroJ150(RegJ005: TRegistroJ005);
    procedure WriteRegistroJ200(RegJ005: TRegistroJ005);
    procedure WriteRegistroJ210(RegJ005: TRegistroJ005);
    procedure WriteRegistroJ215(RegJ210: TRegistroJ210);
    procedure WriteRegistroJ800(RegJ005: TRegistroJ005);
    procedure WriteRegistroJ801(RegJ005: TRegistroJ005);


  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    procedure WriteRegistroJ001;
    procedure WriteRegistroJ005;
//    procedure WriteRegistroJ800;
    procedure WriteRegistroJ900;
    procedure WriteRegistroJ930;
    procedure WriteRegistroJ935;
    procedure WriteRegistroJ990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    property RegistroJ001: TRegistroJ001     read fRegistroJ001 write fRegistroJ001;
    property RegistroJ005: TRegistroJ005List read fRegistroJ005 write fRegistroJ005;
    property RegistroJ800: TRegistroJ800List read fRegistroJ800 write fRegistroJ800;

    property RegistroJ801: TRegistroJ801List read fRegistroJ801 write fRegistroJ801;

    property RegistroJ900: TRegistroJ900     read fRegistroJ900 write fRegistroJ900;
    property RegistroJ930: TRegistroJ930List read fRegistroJ930 write fRegistroJ930;
    property RegistroJ935: TRegistroJ935List read fRegistroJ935 write fRegistroJ935;
    property RegistroJ990: TRegistroJ990     read fRegistroJ990 write fRegistroJ990;
    property RegistroJ100Count: Integer read FRegistroJ100Count write FRegistroJ100Count;
    property RegistroJ150Count: Integer read FRegistroJ150Count write FRegistroJ150Count;
    property RegistroJ200Count: Integer read FRegistroJ200Count write FRegistroJ200Count;
    property RegistroJ210Count: Integer read FRegistroJ210Count write FRegistroJ210Count;
    property RegistroJ215Count: Integer read FRegistroJ215Count write FRegistroJ215Count;
    property RegistroJ800Count: Integer read FRegistroJ800Count write FRegistroJ800Count;
    property RegistroJ801Count: Integer read FRegistroJ801Count write FRegistroJ801Count;

  end;

implementation

{ TBloco_J }

constructor TBloco_J.Create;
begin
  FRegistroJ001 := TRegistroJ001.Create;
  FRegistroJ005 := TRegistroJ005List.Create;
  FRegistroJ800 := TRegistroJ800List.Create;
  FRegistroJ801 := TRegistroJ801List.Create;
  FRegistroJ900 := TRegistroJ900.Create;
  FRegistroJ930 := TRegistroJ930List.Create;
  FRegistroJ935 := TRegistroJ935List.Create;
  FRegistroJ990 := TRegistroJ990.Create;
  FRegistroJ100Count := 0;
  FRegistroJ150Count := 0;
  FRegistroJ800Count := 0;
  FRegistroJ801Count := 0;
  FRegistroJ200Count := 0;
  FRegistroJ210Count := 0;
  FRegistroJ215Count := 0;

  FRegistroJ990.QTD_LIN_J := 0;
end;

destructor TBloco_J.Destroy;
begin
  FRegistroJ001.Free;
  FRegistroJ005.Free;
  FRegistroJ800.Free;
  FRegistroJ801.Free;
  FRegistroJ900.Free;
  FRegistroJ930.Free;
  FRegistroJ935.Free;  
  FRegistroJ990.Free;
  inherited;
end;

procedure TBloco_J.LimpaRegistros;
begin
  FRegistroJ005.Clear;
  FRegistroJ800.Clear;
  FRegistroJ801.Clear;
  FRegistroJ930.Clear;
  FRegistroJ935.Clear;

  FRegistroJ100Count := 0;
  FRegistroJ150Count := 0;
  FRegistroJ200Count := 0;
  FRegistroJ210Count := 0;
  FRegistroJ215Count := 0;
  FRegistroJ800Count := 0;
  FRegistroJ801Count := 0;

  FRegistroJ990.QTD_LIN_J := 0;


end;

procedure  TBloco_J.WriteRegistroJ001;
begin
  if Assigned(FRegistroJ001) then
  begin
     with FRegistroJ001 do
     begin
       Check(((IND_DAD = 0) or (IND_DAD = 1)), '(J-J001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Add( LFill('J001') +
            LFill(IND_DAD, 1) 
            );
       ///
       FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
  end;
end;

procedure TBloco_J.WriteRegistroJ005;
var
intFor: integer;
begin
  if Assigned(FRegistroJ005) then
  begin
     for intFor := 0 to FRegistroJ005.Count - 1 do
     begin
        with FRegistroJ005.Items[intFor] do
        begin
           Check(((ID_DEM = 1) or (ID_DEM = 2)), '(J-J005) Na Identificação das demonstrações, deve ser informado o número 1 ou 2!');
           ///
           Add( LFill('J005') +
                LFill(DT_INI) +
                LFill(DT_FIN) +
                LFill(ID_DEM, 1) +
                LFill(CAB_DEM) 
                );
        end;
        // Registros Filhos
        WriteRegistroJ100(FRegistroJ005.Items[intFor]);
        WriteRegistroJ150(FRegistroJ005.Items[intFor]);
        WriteRegistroJ200(FRegistroJ005.Items[intFor]);
        WriteRegistroJ210(FRegistroJ005.Items[intFor]);
        WriteRegistroJ800(FRegistroJ005.Items[intFor]);
        WriteRegistroJ801(FRegistroJ005.Items[intFor]);

        FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
  end;
end;

procedure TBloco_J.WriteRegistroJ100(RegJ005: TRegistroJ005);
var
intFor: integer;
begin
  if Assigned(RegJ005.RegistroJ100) then
  begin
     for intFor := 0 to RegJ005.RegistroJ100.Count - 1 do
     begin
        with RegJ005.RegistroJ100.Items[intFor] do
        begin
           ///
           Check(((IND_GRP_BAL = '1') or (IND_GRP_BAL = '2')), '(J-J100) No Indicador de grupo do balanço, deve ser informado o número 1 ou 2!');
           Check(((IND_DC_BAL = 'D') or (IND_DC_BAL = 'C')), '(J-J100) No Indicador da situação do saldo, deve ser informado: D ou C!');
           /// Layout 2 a partir da escrituração ano calendário 2013
           if DT_INI >= EncodeDate(2013,01,01) then
              Check(((IND_DC_BAL_INI = 'D') or (IND_DC_BAL_INI = 'C')), '(J-J100) No Indicador da situação do saldo inicial do código de aglutinação no Balança Patrimonial, deve ser informado: D ou C!');
           ///
           /// Layout 2 a partir da escrituração ano calendário 2013
           if DT_INI >= EncodeDate(2013,01,01) then
           begin
             Add( LFill('J100') +
                  LFill(COD_AGL) +
                  LFill(NIVEL_AGL) +
                  LFill(IND_GRP_BAL, 1) +
                  LFill(DESCR_COD_AGL) +
                  LFill(VL_CTA, 19, 2) +
                  LFill(IND_DC_BAL, 1) +
                  LFill(VL_CTA_INI, 19, 2) +
                  LFill(IND_DC_BAL_INI) 
                  );
           end
            else
             begin
               Add( LFill('J100') +
                    LFill(COD_AGL) +
                    LFill(NIVEL_AGL) +
                    LFill(IND_GRP_BAL, 1) +
                    LFill(DESCR_COD_AGL) +
                    LFill(VL_CTA, 19, 2) +
                    LFill(IND_DC_BAL, 1) 
                    );
             end;
        end;
        FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
     FRegistroJ100Count := FRegistroJ100Count + RegJ005.RegistroJ100.Count;
  end;
end;

procedure TBloco_J.WriteRegistroJ150(RegJ005: TRegistroJ005);
var
intFor: integer;
begin
  if Assigned(RegJ005.RegistroJ150) then
  begin
     for intFor := 0 to RegJ005.RegistroJ150.Count - 1 do
     begin
        with RegJ005.RegistroJ150.Items[intFor] do
        begin
           Check(((IND_VL = 'D') or (IND_VL = 'R') or (IND_VL = 'P') or (IND_VL = 'N')), '(J-J150) No Indicador da situação do valor, deve ser informado: D ou R ou P ou N!');

           Check(((IND_VL_ULT_DRE = 'D') or (IND_VL_ULT_DRE = 'R') or (IND_VL_ULT_DRE = 'P') or (IND_VL_ULT_DRE = 'N') or (IND_VL_ULT_DRE = '') ), '(J-J150) No Indicador da situação do saldo valor inicial, deve ser informado: D ou R ou P ou N!');

           ///
           Add( LFill('J150') +
                LFill(COD_AGL) +
                LFill(NIVEL_AGL) +
                LFill(DESCR_COD_AGL) +
                LFill(VL_CTA, 19, 2) +
                LFill(IND_VL, 1) +
                LFill(VL_CTA_ULT_DRE, 19, 2, True) +
                LFill(IND_VL_ULT_DRE, 1, True)
                );

        end;
        FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
     FRegistroJ150Count := FRegistroJ150Count + RegJ005.RegistroJ150.Count;
  end;
end;

//Registro incluído a partir do layout 2
procedure TBloco_J.WriteRegistroJ200(RegJ005: TRegistroJ005);
var
intFor: integer;
begin
  if Assigned(RegJ005.RegistroJ200) then
  begin
     for intFor := 0 to RegJ005.RegistroJ200.Count - 1 do
     begin
        with RegJ005.RegistroJ200.Items[intFor] do
        begin
           ///
           Check(((COD_HIST_FAT <> '')), '(J-J200) Código do histórico do fato contábil deve ser informado!');
           Check(((DESC_FAT <> '')), '(J-J200) Descrição do fato contábil deve ser informado!');
           Add( LFill('J200') +
                LFill(COD_HIST_FAT) +
                LFill(DESC_FAT) 
                );

        end;
        FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
     FRegistroJ200Count := FRegistroJ200Count + RegJ005.RegistroJ200.Count;
  end;
end;

/// J210 - Layout 2 a partir da escrituração ano calendário 2013
procedure TBloco_J.WriteRegistroJ210(RegJ005: TRegistroJ005);
var
intFor: integer;
begin
  if Assigned(RegJ005.RegistroJ210) then
  begin
     for intFor := 0 to RegJ005.RegistroJ210.Count - 1 do
     begin
        with RegJ005.RegistroJ210.Items[intFor] do
        begin
           ///
           Check(((IND_TIP = '0') or (IND_TIP = '1')), '(J-J210) No Indicador do tipo de demonstração, deve ser informado o número 0 ou 1!');
           Check(((IND_DC_CTA = 'D') or (IND_DC_CTA = 'C')), '(J-J210) No Indicador da situação do saldo final informado no campo anterior, deve ser informado: D ou C!');
           Check(((IND_DC_CTA_INI = 'D') or (IND_DC_CTA_INI = 'C')), '(J-J210) Indicador da situação do saldo inicial informado no campo anterior, deve ser informado: D ou C!');
           ///
           Add( LFill('J210') +
                LFill(IND_TIP) +
                LFill(COD_AGL) +
                LFill(DESCR_COD_AGL) +
                LFill(VL_CTA, 19, 2) +
                LFill(IND_DC_CTA) +
                LFill(VL_CTA_INI, 19, 2) +
                LFill(IND_DC_CTA_INI) 
                );
        end;
        WriteRegistroJ215(RegJ005.RegistroJ210.Items[intFor]);

        FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
     FRegistroJ210Count := FRegistroJ210Count + RegJ005.RegistroJ210.Count;
  end;
end;

procedure TBloco_J.WriteRegistroJ215(RegJ210: TRegistroJ210);
var
intFor: integer;
begin
  if Assigned(RegJ210.RegistroJ215) then
  begin
     for intFor := 0 to RegJ210.RegistroJ215.Count - 1 do
     begin
        with RegJ210.RegistroJ215.Items[intFor] do
        begin
           ///
           Check(((COD_HIST_FAT <> '')), '(J-J215) Código do histórico do fato contábil deve ser informado!');
           Check(((IND_DC_FAT = 'D') or (IND_DC_FAT = 'C') or (IND_DC_FAT = 'P') or (IND_DC_FAT = 'N')), '(J-J215) Indicador de situação do saldo informado, deve ser informado: D ou C ou P ou N!');

           Add( LFill('J215') +
                LFill(COD_HIST_FAT) +
                LFill(VL_FAT_CONT, 19, 2) +
                LFill(IND_DC_FAT) 
                );
        end;
        FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
     FRegistroJ215Count := FRegistroJ215Count + RegJ210.RegistroJ215.Count;
  end;
end;

{
procedure TBloco_J.WriteRegistroJ800;
var
intFor: integer;
begin
  if Assigned(FRegistroJ800) then
  begin
     for intFor := 0 to FRegistroJ800.Count - 1 do
     begin
        with FRegistroJ800.Items[intFor] do
        begin
           ///
           Add( LFill('J800') +
                LFill(ARQ_RTF) +
                LFill('J800FIM')
                );
        end;
        FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
  end;
end;
}

procedure TBloco_J.WriteRegistroJ800(RegJ005: TRegistroJ005);
var
intFor: integer;
begin
  if Assigned(RegJ005.RegistroJ800) then
  begin
     for intFor := 0 to RegJ005.RegistroJ800.Count - 1 do
     begin
        with RegJ005.RegistroJ800.Items[intFor] do
        begin

           if DT_INI >= EncodeDate(2016,01,01) then
             Add( LFill('J800') +
                  LFill(TIPO_DOC) +
                  LFill(DESC_RTF) +
                  LFill(HASH_RTF) +
                  LFill(ARQ_RTF) +
                  LFill('J800FIM')
                  )
           else
             Add( LFill('J800') +
                  LFill(ARQ_RTF) +
                  LFill('J800FIM')
                  );


        end;
        FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
     FRegistroJ800Count := FRegistroJ800Count + RegJ005.RegistroJ800.Count;
  end;
end;

procedure TBloco_J.WriteRegistroJ801(RegJ005: TRegistroJ005);
var
intFor: integer;
begin
  if Assigned(RegJ005.RegistroJ801) then
  begin
     for intFor := 0 to RegJ005.RegistroJ801.Count - 1 do
     begin
        with RegJ005.RegistroJ801.Items[intFor] do
        begin

           if DT_INI >= EncodeDate(2016,01,01) then
             Add( LFill('J801') +
                  LFill(TIPO_DOC) +
                  LFill(DESC_RTF) +
                  LFill(HASH_RTF) +
                  LFill(ARQ_RTF) +
                  LFill('J801FIM')
                  )
           else
             Add( LFill('J801') +
                  LFill(ARQ_RTF) +
                  LFill('J801FIM')
                  );


        end;
        FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
     FRegistroJ801Count := FRegistroJ801Count + RegJ005.RegistroJ801.Count;
  end;
end;



procedure TBloco_J.WriteRegistroJ900;
var strLinha: String;
begin
  if Assigned(FRegistroJ900) then
  begin
     with FRegistroJ900 do
     begin
       ///
       strLinha := LFill('J900') +
            LFill('TERMO DE ENCERRAMENTO') +
            LFill(NUM_ORD) +
            LFill(NAT_LIVRO) +
            LFill(NOME) +
            LFill('[*******]') +
            LFill(DT_INI_ESCR) +
            LFill(DT_FIN_ESCR);
       ///
       Add(strLinha);              
       FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;       
     end;
  end;
end;

procedure TBloco_J.WriteRegistroJ930;
var
intFor: integer;
begin
  if Assigned(FRegistroJ930) then
  begin
     for intFor := 0 to FRegistroJ930.Count - 1 do
     begin
        with FRegistroJ930.Items[intFor] do
        begin
         /// Layout 2 a partir da escrituração ano calendário 2013
         if DT_INI >= EncodeDate(2016,01,01) then
         begin
             Add( LFill('J930') +
                  LFill(IDENT_NOM) +
                  LFill(IDENT_CPF) +
                  LFill(IDENT_QUALIF) +
                  LFill(COD_ASSIN, 3) +
                  LFill(IND_CRC) +
                  LFill(EMAIL) +
                  LFill(FONE) +
                  LFill(UF_CRC) +
                  LFill(NUM_SEQ_CRC) +
                  LFill(DT_CRC) +
                  LFill(IND_RESP_LEGAL)
                  );
         end else
         /// Layout 2 a partir da escrituração ano calendário 2013
         if DT_INI >= EncodeDate(2013,01,01) then
         begin
             Add( LFill('J930') +
                  LFill(IDENT_NOM) +
                  LFill(IDENT_CPF) +
                  LFill(IDENT_QUALIF) +
                  LFill(COD_ASSIN, 3) +
                  LFill(IND_CRC) +
                  LFill(EMAIL) +
                  LFill(FONE) +
                  LFill(UF_CRC) +
                  LFill(NUM_SEQ_CRC) +
                  LFill(DT_CRC) 
                  );
         end
          else
           begin
             Add( LFill('J930') +
                  LFill(IDENT_NOM) +
                  LFill(IDENT_CPF) +
                  LFill(IDENT_QUALIF) +
                  LFill(COD_ASSIN, 3) +
                  LFill(IND_CRC) 
                  );

           end;
        end;
        FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
  end;
end;

procedure TBloco_J.WriteRegistroJ935;
var
intFor: integer;
begin
  if Assigned(FRegistroJ935) then
  begin
     for intFor := 0 to FRegistroJ935.Count - 1 do
     begin
        with FRegistroJ935.Items[intFor] do
        begin
           Add( LFill('J935') +
                LFill(NOME_AUDITOR) +
                LFill(COD_CVM_AUDITOR)
                );
        end;
        FRegistroJ990.QTD_LIN_J := FRegistroJ990.QTD_LIN_J + 1;
     end;
  end;  
end;

procedure TBloco_J.WriteRegistroJ990;
var strLinha : String;
begin
  if Assigned(FRegistroJ990) then
  begin
     with FRegistroJ990 do
     begin
       QTD_LIN_J := QTD_LIN_J + 1;
       ///
       strLinha := LFill('J990') +
            LFill(QTD_LIN_J, 0);            
       Add(strLinha);
     end;
  end;
end;

end.
