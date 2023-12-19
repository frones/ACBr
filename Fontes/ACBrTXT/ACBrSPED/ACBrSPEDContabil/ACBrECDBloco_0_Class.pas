{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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
|* 06/05/2014: Francinaldo A. da Costa
|*  - Modificações para o layout 2
|* 04/03/2015: Flavio Rubens Massaro Jr.
|* - Modificação para contemplar layout 3 referente ao ano calendario 2014
|* 03/02/2016: Anderson Nunes Kovaski
|* - Modificação para contemplar layout 4 referente ao ano calendario 2015
*******************************************************************************}

unit ACBrECDBloco_0_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrECDBloco_0;

const
  sIndicadorQTDLinhasArquivo = '[*******]';

type
  /// TBLOCO_0 - Abertura, Identificação e Referências
  TBloco_0 = class(TACBrSPED)
  private
    FRegistro0000: TRegistro0000;      /// BLOCO 0 - Registro0000
    FRegistro0001: TRegistro0001;      /// BLOCO 0 - Registro0001
    FRegistro0007: TRegistro0007List;  /// BLOCO 0 - Lista de Registro0007
    FRegistro0020: TRegistro0020List;  /// BLOCO 0 - Lista de Registro0020
    FRegistro0035: TRegistro0035List;  /// BLOCO 0 - Lista de Registro0035
    FRegistro0150: TRegistro0150List;  /// BLOCO 0 - Lista de Registro0150
    //FRegistro0180: TRegistro0180List;  /// BLOCO 0 - Lista de Registro0180
    FRegistro0990: TRegistro0990;      /// BLOCO 0 - FRegistro0990
    FRegistro0180Count: Integer;
//    procedure SetRegistro0035(const Value: TRegistro0035List);      /// BLOCO 0 - FRegistro0990
    procedure WriteRegistro0180(Reg0150: TRegistro0150);
  protected
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0007;
    procedure WriteRegistro0020;
    procedure WriteRegistro0035;
    procedure WriteRegistro0150;
    //procedure WriteRegistro0180;
    procedure WriteRegistro0990;

    property Registro0000: TRegistro0000     read FRegistro0000 write FRegistro0000;
    property Registro0001: TRegistro0001     read FRegistro0001 write FRegistro0001;
    property Registro0007: TRegistro0007List read FRegistro0007 write FRegistro0007;
    property Registro0020: TRegistro0020List read FRegistro0020 write FRegistro0020;
    property Registro0035: TRegistro0035List read FRegistro0035 write FRegistro0035;
    property Registro0150: TRegistro0150List read FRegistro0150 write FRegistro0150;
    //property Registro0180: TRegistro0180List read FRegistro0180 write FRegistro0180;
    property Registro0990: TRegistro0990     read FRegistro0990 write FRegistro0990;
    property Registro0180Count: Integer read FRegistro0180Count write FRegistro0180Count;

  end;

implementation

uses ACBrTXTClass, ACBrTXTUtils;

{ TBloco_0 }

constructor TBloco_0.Create;
begin
  inherited Create;
  FRegistro0000 := TRegistro0000.Create;
  FRegistro0001 := TRegistro0001.Create;
  FRegistro0007 := TRegistro0007List.Create;
  FRegistro0020 := TRegistro0020List.Create;
  FRegistro0035 := TRegistro0035List.Create;
  FRegistro0150 := TRegistro0150List.Create;
  //FRegistro0180 := TRegistro0180List.Create;
  FRegistro0990 := TRegistro0990.Create;

  FRegistro0990.QTD_LIN_0 := 0;
  FRegistro0180Count := 0;
end;

destructor TBloco_0.Destroy;
begin
  FRegistro0000.Free;
  FRegistro0001.Free;
  FRegistro0007.Free;
  FRegistro0020.Free;
  FRegistro0035.Free;  
  FRegistro0150.Free;
  //FRegistro0180.Free;
  FRegistro0990.Free;
  inherited;
end;

procedure TBloco_0.LimpaRegistros;
begin
  FRegistro0007.Clear;
  FRegistro0020.Clear;
  FRegistro0035.Clear;  
  FRegistro0150.Clear;
  //FRegistro0180.Clear;

  FRegistro0990.QTD_LIN_0 := 0;
  FRegistro0180Count := 0;
end;

//procedure TBloco_0.SetRegistro0035(const Value: TRegistro0035List);
//begin
//
//end;

procedure TBloco_0.WriteRegistro0000;
begin

  if Assigned(FRegistro0000) then
  begin
     with FRegistro0000 do
     begin
       Check(NOME <> '', '(0-0000) O nome empresarial é obrigatório!');
       Check(funChecaCNPJ(CNPJ), '(0-0000) O CNPJ "%s" digitado é inválido!', [CNPJ]);
       Check(funChecaUF(UF), '(0-0000) A UF "%s" digitada é inválido!', [UF]);
       Check(funChecaIE(IE, UF), '(0-0000) A inscrição estadual "%s" digitada é inválida!', [IE]);
       Check(funChecaMUN(StrToInt(COD_MUN)), '(0-0000) O código do município "%s" digitado é inválido!', [COD_MUN]);
       if DT_INI >= EncodeDate(2019,01,01) then
       begin
         Check(((IND_CENTRALIZADA >= '0') and (IND_CENTRALIZADA <= '1')), '(0-0000) O indicador "%s" da modalidade de escrituração centralizada ou descentralizada, deve ser informado o número 0 ou 1!', [IND_CENTRALIZADA]);
         Check(((IND_MUDANC_PC >= '0') and (IND_MUDANC_PC <= '1')), '(0-0000) O indicador "%s"  de mudança de plano de contas, deve ser informado o número 0 ou 1!', [IND_MUDANC_PC]);
         Check((((StrToIntDef(COD_PLAN_REF,0) >= 1) and (StrToIntDef(COD_PLAN_REF,0) <= 10)) or (COD_PLAN_REF = '')), '(0-0000) O Còdigo "%s" Código do Plano de Contas Referencial que será utilizado para o mapeamento de todas as contas analíticas, deve ser em branco ou informado o número entre 1 e 10!', [IND_MUDANC_PC]);
       end;
       if DT_INI >= EncodeDate(2014,01,01) then
       begin
         Check(((TIP_ECD >= '0') and (TIP_ECD <= '2')), '(0-0000) O indicador "%s" de tipo ECD, deve ser informado o número 0, 1 ou 2!', [TIP_ECD]);
       end;
       if DT_INI >= EncodeDate(2013,01,01) then
       begin
         Check(((IND_SIT_INI_PER >= '0') and (IND_SIT_INI_PER <= '3')), '(0-0000) O indicador "%s" de situação no início do período, deve ser informado o número 0 ou 1 ou 2 ou 3!', [IND_SIT_INI_PER]);
         Check(((IND_NIRE >= '0') and (IND_NIRE <= '1')), '(0-0000) O indicador "%s" de existência de NIRE, deve ser informado o número 0 ou 1!', [IND_NIRE]);
         Check(((IND_FIN_ESC >= '0') and (IND_FIN_ESC <= '3')), '(0-0000) O indicador "%s" da finalidade da escrituração, deve ser informado o número 0 ou 1 ou 2 ou 3!', [IND_FIN_ESC]);
         Check(((IND_EMP_GRD_PRT >= '0') and (IND_EMP_GRD_PRT <= '1')), '(0-0000) O indicador "%s" de empresa de grande porte, deve ser informado o número 0 ou 1!', [IND_EMP_GRD_PRT]);
       end;

       // Layout 8 a partir da escrituração ano calendário 2019
       if DT_INI >= EncodeDate(2019,01,01) then
       begin
         Add( LFill('0000') +
              LFill('LECD') +
              LFill(DT_INI) +
              LFill(DT_FIN) +
              LFill(NOME) +
              LFill(CNPJ) +
              LFill(UF) +
              LFill(IE) +
              LFill(COD_MUN, 7) +
              LFill(IM) +
              LFill(IND_SIT_ESP, 0, True) +
              LFill(IND_SIT_INI_PER) +
              LFill(IND_NIRE) +
              LFill(IND_FIN_ESC) +
              LFill(COD_HASH_SUB) +
              LFill(IND_EMP_GRD_PRT) +
              LFill(TIP_ECD) +
              LFill(COD_SCP) +
              LFill(IDENT_MF) +
              LFill(IND_ESC_CONS) +
              LFill(IND_CENTRALIZADA) +
              LFill(IND_MUDANC_PC) +
              LFill(COD_PLAN_REF)
              );
       end
       // Layout 5 a partir da escrituração ano calendário 2016
       else if DT_INI >= EncodeDate(2016,01,01) then
       begin
         Add( LFill('0000') +
              LFill('LECD') +
              LFill(DT_INI) +
              LFill(DT_FIN) +
              LFill(NOME) +
              LFill(CNPJ) +
              LFill(UF) +
              LFill(IE) +
              LFill(COD_MUN, 7) +
              LFill(IM) +
              LFill(IND_SIT_ESP, 0, True) +
              LFill(IND_SIT_INI_PER) +
              LFill(IND_NIRE) +
              LFill(IND_FIN_ESC) +
              LFill(COD_HASH_SUB) +
//              LFill(NIRE_SUBST) +
              LFill(IND_EMP_GRD_PRT) +
              LFill(TIP_ECD) +
              LFill(COD_SCP) +
              LFill(IDENT_MF) +
              LFill(IND_ESC_CONS));
       end
       // Layout 4 a partir da escrituração ano calendário 2015
       else if DT_INI >= EncodeDate(2015,01,01) then
       begin
         Add( LFill('0000') +
              LFill('LECD') +
              LFill(DT_INI) +
              LFill(DT_FIN) +
              LFill(NOME) +
              LFill(CNPJ) +
              LFill(UF) +
              LFill(IE) +
              LFill(COD_MUN, 7) +
              LFill(IM) +
              LFill(IND_SIT_ESP, 0, True) +
              LFill(IND_SIT_INI_PER) +
              LFill(IND_NIRE) +
              LFill(IND_FIN_ESC) +
              LFill(COD_HASH_SUB) +
              LFill(NIRE_SUBST) +
              LFill(IND_EMP_GRD_PRT) +
              LFill(TIP_ECD) +
              LFill(COD_SCP) +
              LFill(IDENT_MF));
       end
       /// Layout 3 a partir da escrituração ano calendário 2014
       else if DT_INI >= EncodeDate(2014,01,01) then
         begin
           Add( LFill('0000') +
                LFill('LECD') +
                LFill(DT_INI) +
                LFill(DT_FIN) +
                LFill(NOME) +
                LFill(CNPJ) +
                LFill(UF) +
                LFill(IE) +
                LFill(COD_MUN, 7) +
                LFill(IM) +
                LFill(IND_SIT_ESP, 0, True) +
                LFill(IND_SIT_INI_PER) +
                LFill(IND_NIRE) +
                LFill(IND_FIN_ESC) +
                LFill(COD_HASH_SUB) +
                LFill(NIRE_SUBST) +
                LFill(IND_EMP_GRD_PRT) +
                LFill(TIP_ECD) +
                LFill(COD_SCP));
         end
       /// Layout 2 a partir da escrituração ano calendário 2013
       else if DT_INI >= EncodeDate(2013,01,01) then
       begin
         Add( LFill('0000') +
              LFill('LECD') +
              LFill(DT_INI) +
              LFill(DT_FIN) +
              LFill(NOME) +
              LFill(CNPJ) +
              LFill(UF) +
              LFill(IE) +
              LFill(COD_MUN, 7) +
              LFill(IM) +
              LFill(IND_SIT_ESP, 0, True) +
              LFill(IND_SIT_INI_PER) +
              LFill(IND_NIRE) +
              LFill(IND_FIN_ESC) +
              LFill(COD_HASH_SUB) +
              LFill(NIRE_SUBST) +
              LFill(IND_EMP_GRD_PRT));
       end
        else
         begin
           Add( LFill('0000') +
                LFill('LECD') +
                LFill(DT_INI) +
                LFill(DT_FIN) +
                LFill(NOME) +
                LFill(CNPJ) +
                LFill(UF) +
                LFill(IE) +
                LFill(COD_MUN, 7) +
                LFill(IM) +
                LFill(IND_SIT_ESP, 0, True)
                );
         end;
       ///
       FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0001;
begin
  if Assigned(FRegistro0001) then
  begin
     with FRegistro0001 do
     begin
       Check(((IND_DAD = 0) or (IND_DAD = 1)), '(0-0001) ABERTURA DO BLOCO: Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Add( LFill('0001') +
            LFill(IND_DAD, 1)
            );
       ///
       FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0007;
var
intFor: integer;
begin
  if Assigned(FRegistro0007) then
  begin
     for intFor := 0 to FRegistro0007.Count - 1 do
     begin
        with FRegistro0007.Items[intFor] do
        begin
           ///
           Add( LFill('0007') +
                LFill(COD_ENT_REF) +
                LFill(COD_INSCR)
                );
        end;
        FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0020;
var
intFor: integer;
begin
  if Assigned(FRegistro0020) then
  begin
     for intFor := 0 to FRegistro0020.Count - 1 do
     begin
        with FRegistro0020.Items[intFor] do
        begin
           Check(((IND_DEC = 0) or (IND_DEC = 1)), '(0-0020) O Indicador de descentralização, deve ser informado o número 0 ou 1!');
           Check(funChecaCNPJ(CNPJ), '(0-0020) O CNPJ "%s" digitado é inválido!', [CNPJ]);
           Check(funChecaUF(UF), '(0-0020) A UF "%s" digitada é inválido!', [UF]);
           Check(funChecaIE(IE, UF), '(0-0020) A inscrição estadual "%s" digitada é inválida!', [IE]);
           Check(funChecaMUN(StrToInt(COD_MUN)), '(0-0020) O código do município "%s" digitado é inválido!', [COD_MUN]);
           ///
           Add( LFill('0020') +
                LFill(IND_DEC, 1) +
                LFill(CNPJ) +
                LFill(UF) +
                LFill(IE) +
                LFill(COD_MUN, 7) +
                LFill(IM) +
                LFill(NIRE, 11)
                );
        end;
        FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0035;
var
intFor: integer;
begin

  if Assigned(FRegistro0035) then
  begin
     for intFor := 0 to FRegistro0035.Count - 1 do
     begin
        with FRegistro0035.Items[intFor] do
        begin
           Check(COD_SCP <> '', '(0-0035) O código do SCP é obrigatório!');
           Check(NOME_SCP <> '', '(0-0035) O nome do SCP é obrigatório!');
           ///
           Add( LFill('0035') +
                LFill(COD_SCP) +
                LFill(NOME_SCP)
				);
        end;
        FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
     end;
  end;  
end;

procedure TBloco_0.WriteRegistro0150;
var
intFor: integer;
begin
  if Assigned(FRegistro0150) then
  begin
     for intFor := 0 to FRegistro0150.Count - 1 do
     begin
        with FRegistro0150.Items[intFor] do
        begin
           Check(NOME <> '', '(0-0150) O nome do participante é obrigatório!');
           Check(funChecaPAISIBGE(COD_PAIS), '(0-0150) %s-%s, o código do país "%s" digitado é inválido!', [COD_PART, NOME, COD_PAIS]);
           if Length(CNPJ) > 0 then
              Check(funChecaCNPJ(CNPJ), '(0-0150) %s-%s, o CNPJ "%s" digitado é inválido!', [COD_PART, NOME, CNPJ]);
           if Length(CPF)  > 0 then
              Check(funChecaCPF(CPF), '(0-0150) %s-%s, o CPF "%s" digitado é inválido!', [COD_PART, NOME, CPF]);
//           Check(funChecaUF(UF), '(0-0150) A UF "%s" digitada é inválido!', [UF]);
//           Check(funChecaIE(IE, UF), '(0-0150) %s-%s, a Inscrição Estadual "%s" digitada é inválida!', [COD_PART, NOME, IE]);
           Check(funChecaMUN(COD_MUN), '(0-0150) %s-%s, o código do município "%s" digitado é inválido!', [COD_PART, NOME, IntToStr(COD_MUN)]);
           ///
           Add( LFill('0150') +
                LFill(COD_PART) +
                LFill(NOME) +
                LFill(COD_PAIS, 5) +
                LFill(CNPJ) +
                LFill(CPF) +
                LFill(NIT, 11) +
                LFill(UF) +
                LFill(IE) +
                LFill(IE_ST) +
                LFill(COD_MUN, 7) +
                LFill(IM) +
                LFill(SUFRAMA, 9)
                );

           WriteRegistro0180(FRegistro0150.Items[intFor]);

        end;
        FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
     end;
  end;
end;


procedure TBloco_0.WriteRegistro0180(Reg0150: TRegistro0150);
var
  intFor: Integer;
begin

  if Assigned(Reg0150.Registro0180) then
  begin
    for intFor := 0 to Reg0150.Registro0180.Count - 1 do
    begin
      with Reg0150.Registro0180.Items[intFor] do
      begin
        Check(((COD_REL >= '01') and (COD_REL <= '11')), '(0-0180) O código "%s" de relacionamento, deve ser informado o número na faixa de 01 até 11!', [COD_REL]);
        Check(DT_INI_REL > 0, '(0-0180) A data do inicio relacionamento é inválida!');
        ///
        Add(LFill('0180') + LFill(COD_REL, 2) + LFill(DT_INI_REL) + LFill(DT_FIN_REL, 'ddmmyyyy'));
      end;
      FRegistro0990.QTD_LIN_0 := FRegistro0990.QTD_LIN_0 + 1;
    end;
      FRegistro0180Count := FRegistro0180Count + Reg0150.Registro0180.Count;
  end;

end;

procedure TBloco_0.WriteRegistro0990;
var strLinha : String;
begin
  if Assigned(FRegistro0990) then
  begin
     with FRegistro0990 do
     begin
       QTD_LIN_0 := QTD_LIN_0 + 1;
       ///
       strLinha:= LFill('0990') +
                 LFill(QTD_LIN_0, 0);
       Add(strLinha);
     end;
  end;
end;

end.
