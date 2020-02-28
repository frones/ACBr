{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro e Renato Rubinho                }
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

unit ACBrECDBloco_K_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrECDBloco_K, ACBrECDBloco_0_Class;

type
  /// TBloco_K -
  TBloco_K = class(TACBrSPED)
  private
    FRegistroK001: TRegistroK001;      /// BLOCO K - RegistroK001
    FRegistroK030: TRegistroK030;      /// BLOCO K - RegistroK030
    FRegistroK990: TRegistroK990;      /// BLOCO K - FRegistroK990

    FRegistroK100Count: Integer;
    FRegistroK110Count: Integer;
    FRegistroK115Count: Integer;
    FRegistroK200Count: Integer;
    FRegistroK210Count: Integer;
    FRegistroK300Count: Integer;
    FRegistroK310Count: Integer;
    FRegistroK315Count: Integer;

    FBloco_0: TBloco_0;
    procedure WriteRegistroK100(RegK030: TRegistroK030);
    procedure WriteRegistroK110(RegK100: TRegistroK100);
    procedure WriteRegistroK115(RegK110: TRegistroK110);
    procedure WriteRegistroK200(RegK030: TRegistroK030);
    procedure WriteRegistroK210(RegK200: TRegistroK200);
    procedure WriteRegistroK300(RegK030: TRegistroK030);
    procedure WriteRegistroK310(RegK300: TRegistroK300);
    procedure WriteRegistroK315(RegK310: TRegistroK310);
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    procedure WriteRegistroK001;
    procedure WriteRegistroK030;
    procedure WriteRegistroK990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroK001: TRegistroK001     read fRegistroK001 write fRegistroK001;
    property RegistroK030: TRegistroK030     read fRegistroK030 write fRegistroK030;
    property RegistroK990: TRegistroK990     read fRegistroK990 write fRegistroK990;

    property RegistroK100Count: Integer read FRegistroK100Count write FRegistroK100Count;
    property RegistroK110Count: Integer read FRegistroK110Count write FRegistroK110Count;
    property RegistroK115Count: Integer read FRegistroK115Count write FRegistroK115Count;
    property RegistroK200Count: Integer read FRegistroK200Count write FRegistroK200Count;
    property RegistroK210Count: Integer read FRegistroK210Count write FRegistroK210Count;
    property RegistroK300Count: Integer read FRegistroK300Count write FRegistroK300Count;
    property RegistroK310Count: Integer read FRegistroK310Count write FRegistroK310Count;
    property RegistroK315Count: Integer read FRegistroK315Count write FRegistroK315Count;
  end;

implementation

uses ACBrTXTUtils;

{ TBloco_K }

constructor TBloco_K.Create;
begin
  inherited Create;
  FRegistroK001 := TRegistroK001.Create;
  FRegistroK030 := TRegistroK030.Create;
  FRegistroK990 := TRegistroK990.Create;
  FRegistroK100Count := 0;
  FRegistroK110Count := 0;
  FRegistroK115Count := 0;
  FRegistroK200Count := 0;
  FRegistroK210Count := 0;
  FRegistroK300Count := 0;
  FRegistroK310Count := 0;
  FRegistroK315Count := 0;

  FRegistroK990.QTD_LIN_K := 0;
end;

destructor TBloco_K.Destroy;
begin
  FRegistroK001.Free;
  FRegistroK030.Free;
  FRegistroK990.Free;
  inherited;
end;

procedure TBloco_K.LimpaRegistros;
begin
  FRegistroK100Count := 0;
  FRegistroK110Count := 0;
  FRegistroK115Count := 0;
  FRegistroK200Count := 0;
  FRegistroK210Count := 0;
  FRegistroK300Count := 0;
  FRegistroK310Count := 0;
  FRegistroK315Count := 0;

  FRegistroK990.QTD_LIN_K := 0;
end;

procedure  TBloco_K.WriteRegistroK001;
begin
  if Assigned(FRegistroK001) then
  begin
     with FRegistroK001 do
     begin
       Check(((IND_DAD = 0) or (IND_DAD = 1)), '(K-K001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Add( LFill('K001') +
            LFill(IND_DAD, 1)
            );
       ///
       FRegistroK990.QTD_LIN_K := FRegistroK990.QTD_LIN_K + 1;
     end;
  end;
end;

procedure TBloco_K.WriteRegistroK030;
var strLinha: String;
begin
  if Assigned(FRegistroK030) then
  begin
     with FRegistroK030 do
     begin
        if ( Trunc(DT_INI) > 0  ) then
        begin
           ///
           strLinha := LFill('K030') +
                LFill(DT_INI) +
                LFill(DT_FIN);
           ///
           Add(strLinha);

           FRegistroK990.QTD_LIN_K := FRegistroK990.QTD_LIN_K + 1;

           // Registro Filho
           WriteRegistroK100(FRegistroK030);
           // Registro Filho
           WriteRegistroK200(FRegistroK030);
           // Registro Filho
           WriteRegistroK300(FRegistroK030);
        end;
     end;
  end;
end;

procedure TBloco_K.WriteRegistroK100(RegK030: TRegistroK030);
var
intFor: integer;
begin
  if Assigned(RegK030.RegistroK100) then
  begin
     for intFor := 0 to RegK030.RegistroK100.Count - 1 do
     begin
        with RegK030.RegistroK100.Items[intFor] do
        begin
           Check(funChecaPAISIBGE(COD_PAIS), '(K-K100) %s-%s, o código do país "%s" digitado é inválido!', [EMP_COD, NOME, COD_PAIS]);
           Check(EMP_COD <> '', '(K-K100) Código de identificação da empresa participante é obrigatório!');
           Check(NOME <> '', '(K-K100) O nome empresarial é obrigatório!');
           Check(((EVENTO = 'S') or (EVENTO = 'N')), '(K-K100) No Indicador de evento societário ocorrido no período, deve ser informado: S ou N!');

           Add( LFill('K100') +
                LFill(COD_PAIS, 5) +
                LFill(EMP_COD, 4) +
                LFill(CNPJ) +
                LFill(NOME) +
                LFill(PER_PART, 8, 4) +
                LFill(EVENTO) +
                LFill(PER_CONS, 8, 4) +
                LFill(DATA_INI_EMP) +
                LFill(DATA_FIN_EMP)
                );
        end;
        WriteRegistroK110(RegK030.RegistroK100.Items[intFor]);

        FRegistroK990.QTD_LIN_K := FRegistroK990.QTD_LIN_K + 1;
     end;
     FRegistroK100Count := FRegistroK100Count + RegK030.RegistroK100.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK110(RegK100: TRegistroK100);
var
intFor: integer;
begin
  if Assigned(RegK100.RegistroK110) then
  begin
     for intFor := 0 to RegK100.RegistroK110.Count - 1 do
     begin
        with RegK100.RegistroK110.Items[intFor] do
        begin
           ///
           Check(((EVENTO = '1') or (EVENTO = '2') or (EVENTO = '3') or (EVENTO = '4') or
                  (EVENTO = '5') or (EVENTO = '6') or (EVENTO = '7') or (EVENTO = '8')), '(J-K110) Indicador de evento societário ocorrido no período, deve ser informado: 1, 2, 3, 4, 5, 6, 7 ou 8!');

           Add( LFill('K110') +
                LFill(EVENTO) +
                LFill(DT_EVENTO)
                );
        end;
        // Registro Filho
        WriteRegistroK115(RegK100.RegistroK110.Items[intFor]);

        FRegistroK990.QTD_LIN_K := FRegistroK990.QTD_LIN_K + 1;
     end;
     FRegistroK110Count := FRegistroK110Count + RegK100.RegistroK110.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK115(RegK110: TRegistroK110);
var
intFor: integer;
begin
  if Assigned(RegK110.RegistroK115) then
  begin
     for intFor := 0 to RegK110.RegistroK115.Count - 1 do
     begin
        with RegK110.RegistroK115.Items[intFor] do
        begin
           Check(((COND_PART = '1') or (COND_PART = '2') or (COND_PART = '3')), '(K-K115) No Indicador de condição da empresa relacionada à operação, deve ser informado: 1 ou 2 ou 3!');
           ///
           Add( LFill('K115') +
                LFill(EMP_COD_PART, 4) +
                LFill(COND_PART, 1) +
                LFill(PER_EVT, 8, 4)
                );
        end;

        FRegistroK990.QTD_LIN_K := FRegistroK990.QTD_LIN_K + 1;
     end;
     FRegistroK115Count := FRegistroK115Count + RegK110.RegistroK115.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK200(RegK030: TRegistroK030);
var
intFor: integer;
begin
  if Assigned(RegK030.RegistroK200) then
  begin
     for intFor := 0 to RegK030.RegistroK200.Count - 1 do
     begin
        with RegK030.RegistroK200.Items[intFor] do
        begin
           Check(((IND_CTA = 'S') or (IND_CTA = 'A')), '(K-K200) Indicador do tipo de conta deve ser S - Sintética ou A - Analítica');
           ///
           Add( LFill('K200') +
                LFill(COD_NAT) +
                LFill(IND_CTA) +
                LFill(NIVEL) +
                LFill(COD_CTA) +
                LFill(COD_CTA_SU) +
                LFill(CTA)
                );
        end;
        // Registro Filho
        WriteRegistroK210(RegK030.RegistroK200.Items[intFor]);

        FRegistroK990.QTD_LIN_K := FRegistroK990.QTD_LIN_K + 1;
     end;
     FRegistroK200Count := FRegistroK200Count + RegK030.RegistroK200.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK210(RegK200: TRegistroK200);
var
intFor: integer;
begin
  if Assigned(RegK200.RegistroK210) then
  begin
     for intFor := 0 to RegK200.RegistroK210.Count - 1 do
     begin
        with RegK200.RegistroK210.Items[intFor] do
        begin
           ///
           Add( LFill('K210') +
                LFill(COD_EMP, 4) +
                LFill(COD_CTA_EMP)
                );
        end;

        FRegistroK990.QTD_LIN_K := FRegistroK990.QTD_LIN_K + 1;
     end;
     FRegistroK210Count := FRegistroK210Count + RegK200.RegistroK210.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK300(RegK030: TRegistroK030);
var
intFor: integer;
begin
  if Assigned(RegK030.RegistroK300) then
  begin
     for intFor := 0 to RegK030.RegistroK300.Count - 1 do
     begin
        with RegK030.RegistroK300.Items[intFor] do
        begin
           Check(((IND_VAL_AG = 'D') or (IND_VAL_AG = 'C')), '(K-K300) Indicador da situação do valor aglutinado de ser D – Devedor ou C – Credor');
           Check(((IND_VAL_EL = 'D') or (IND_VAL_EL = 'C')), '(K-K300) Indicador da situação do valor eliminado de ser D – Devedor ou C – Credor');
           Check(((IND_VAL_CS = 'D') or (IND_VAL_CS = 'C')), '(K-K300) Indicador da situação do valor consolidado de ser D – Devedor ou C – Credor');
           ///
           Add( LFill('K300') +
                LFill(COD_CTA) +
                LFill(VAL_AG, 19, 2) +
                LFill(IND_VAL_AG) +
                LFill(VAL_EL, 19, 2) +
                LFill(IND_VAL_EL) +
                LFill(VAL_CS, 19, 2) +
                LFill(IND_VAL_CS)
                );
        end;
        // Registro Filho
        WriteRegistroK310(RegK030.RegistroK300.Items[intFor]);

        FRegistroK990.QTD_LIN_K := FRegistroK990.QTD_LIN_K + 1;
     end;
     FRegistroK300Count := FRegistroK300Count + RegK030.RegistroK300.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK310(RegK300: TRegistroK300);
var
intFor: integer;
begin
  if Assigned(RegK300.RegistroK310) then
  begin
     for intFor := 0 to RegK300.RegistroK310.Count - 1 do
     begin
        with RegK300.RegistroK310.Items[intFor] do
        begin
           Check((IND_VALOR = 'D') or (IND_VALOR = 'C'), '(K-K310) No Indicador da situação do valor eliminado, deve ser informado: D ou C!');
           ///
           Add( LFill('K310') +
                LFill(EMP_COD_PARTE, 4) +
                LFill(VALOR, 19, 2) +
                LFill(IND_VALOR)
                );
        end;
        // Registro Filho
        WriteRegistroK315(RegK300.RegistroK310.Items[intFor]);

        FRegistroK990.QTD_LIN_K := FRegistroK990.QTD_LIN_K + 1;
     end;
     FRegistroK310Count := FRegistroK310Count + RegK300.RegistroK310.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK315(RegK310: TRegistroK310);
var
intFor: integer;
begin
  if Assigned(RegK310.RegistroK315) then
  begin
     for intFor := 0 to RegK310.RegistroK315.Count - 1 do
     begin
        with RegK310.RegistroK315.Items[intFor] do
        begin
           Check(((IND_VALOR = 'D') or (IND_VALOR = 'C')), '(K-K315) No indicador da situação do valor eliminado, deve ser informado: D ou C!');
           ///
           Add( LFill('K315') +
                LFill(EMP_COD_CONTRA, 4) +
                LFill(COD_CONTRA) +
                LFill(VALOR, 19, 2) +
                LFill(IND_VALOR)
                );
        end;

        FRegistroK990.QTD_LIN_K := FRegistroK990.QTD_LIN_K + 1;
     end;
     FRegistroK315Count := FRegistroK315Count + RegK310.RegistroK315.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK990;
var strLinha : String;
begin
  if Assigned(FRegistroK990) then
  begin
     with FRegistroK990 do
     begin
       QTD_LIN_K := QTD_LIN_K + 1;
       ///
       strLinha := LFill('K990') +
            LFill(QTD_LIN_K, 0);
       Add(strLinha);
     end;
  end;
end;

end.

