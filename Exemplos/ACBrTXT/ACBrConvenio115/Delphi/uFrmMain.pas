{******************************************************************************}
{ Projeto: TFrmACBrConvenio115_PRN                                             }
{                                                                              }
{ Função: Imprimir Nota Fiscal Modelo 21/22 a partir dos dados inseridos no    }
{         componente TACBrConvenio115                                          }
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
{******************************************************************************}

{******************************************************************************}
{ Direitos Autorais Reservados © 2013 - Jéter Rabelo Ferreira                  }
{ Contato: jeter.rabelo@jerasoft.com.br                                        }
{******************************************************************************}
unit uFrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ACBrConvenio115,
  uFrmACBrConvenio115_PRN;

type
  TForm3 = class(TForm)
    Button1: TButton;
    ACBrConvenio115: TACBrConvenio115;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  FileCtrl, RLConsts;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  ADir: string;
  OMestre: TACBrConvenio115Mestre;
  ODetalhe: TACBrConvenio115Item;
  I: Integer;
  A: Integer;
begin
// Informações de LayOut, Validador e etc verificar o cabeçalho do arquivo
// ACBrConvenio115.pas

  if not SelectDirectory('Selecione uma pasta para salvar os arquivos', '', ADir, [sdNewFolder, sdNewUI]) then
    Exit;

  with ACBrConvenio115 do
  begin
    SalvarEm := ADir;
    UF := 'MG';
    Serie := '001';
    Ano := 2013;
    Mes := 02;
//  O Responsável não será utilizado, pois é necessário para a geração do arquivo
//  totalizador. Esse arquivo o programa validador cria
//    with Responsavel do
//    begin
//      Nome := 'Jéter Rabelo Ferreira';
//      Cargo := 'Contador';
//      EMail := 'jeter.rabelo@jerasoft.com.br';
//    end;

    for I := 0 to 3 do
    begin
      OMestre := TACBrConvenio115Mestre.Create;
      OMestre.Destinatario.CnpjCpf := '123.456.789-09';
      OMestre.Destinatario.InscricaoEstadual := '1234567890';
      OMestre.Destinatario.RazaoSocial := 'Jéter Rabelo Ferreira';
      OMestre.Destinatario.Logradouro := 'Av. Antonio Carlos';
      OMestre.Destinatario.Numero := '150';
      OMestre.Destinatario.Complemento := '';
      OMestre.Destinatario.CEP := '37730-000';
      OMestre.Destinatario.Bairro := 'Centro';
      OMestre.Destinatario.Municipio := 'Campestre';
      OMestre.Destinatario.UF := 'MG';
      OMestre.Destinatario.Telefone := '3537430000';
      OMestre.Destinatario.CodigoConsumidor := '1001'; // Código do Cliente
      OMestre.TipoAssinante := tac11182PessoaFisicaResidencial;
      OMestre.TipoUtilizacao := pc112ProvimentoAcessoInternet;
      OMestre.DataEmissao := StrToDate('01/01/2013');
      OMestre.Modelo := 21;
      OMestre.Serie := '001';
      OMestre.NumeroNF := I + 1;
      OMestre.ValorTotal := 400;
      OMestre.ICMS_BaseCalculo := 0;
      OMestre.ICMS_Valor := 0;
      OMestre.IsentosNaoTributadas := 0;
      OMestre.OutrosValores := 0;
      OMestre.IsentosNaoTributadas := 0;
      OMestre.AnoMesRefencia := '1212'; // AAMM
      for A := 0 to 3 do
      begin
        // Será informado apenas 1 item para exemplo, por isso o comando FOR está comentado
        // Informar todos os detalhes (Items) que compõem a NF (Serviço/Juros e etc)
        ODetalhe := TACBrConvenio115Item.Create;
        ODetalhe.CFOP := '5307';
        ODetalhe.Item := 1;
        ODetalhe.CodigoServico := '1' ;
        ODetalhe.DescricaoServico := 'Provimento de acesso a internet';
        ODetalhe.Unidade := '';
        ODetalhe.ClassificacaoItem := '1001' ; // Verificar tabela 11.5 do convênio
        ODetalhe.QtdeContratada := 0;
        ODetalhe.QtdePrestada := 0;
        ODetalhe.ValorTotal := 100;
        ODetalhe.Desconto := 0;
        ODetalhe.AcrescimosDespAcessorias := 0;
        ODetalhe.ICMSBaseCalculo := 0;
        ODetalhe.ICMSValor := 0;
        ODetalhe.ICMSAliquota := 0;
        ODetalhe.IsentoNaoTributados := 0;
        ODetalhe.OutrosValores := 0;
        ODetalhe.AnoMesApuracao := '1212';
        OMestre.Detalhes.Add(ODetalhe);
      end;
      Mestre.Add(OMestre);
    end;
    Gerar;
    MessageDlg('Término do processo!', mtInformation, [mbOK], 0);
  end;
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  ADir: string;
  OMestre: TACBrConvenio115Mestre;
  ODetalhe: TACBrConvenio115Item;
  I: Integer;
  A: Integer;
  FrmPRN: TFrmACBrConvenio115_PRN;
begin
  FrmPRN := TFrmACBrConvenio115_PRN.Create(nil);
  try
    with FrmPRN do
    begin
      with Convenio115 do
      begin
        Clear;
        SalvarEm := ADir;
        UF := 'MG';
        Serie := '001';
        Ano := 2013;
        Mes := 02;
        for I := 0 to 0 do
        begin
          OMestre := TACBrConvenio115Mestre.Create;
          OMestre.Destinatario.CnpjCpf := '123.456.789-09';
          OMestre.Destinatario.InscricaoEstadual := '1234567890';
          OMestre.Destinatario.RazaoSocial := 'Jéter Rabelo Ferreira';
          OMestre.Destinatario.Logradouro := 'Av. Antonio Carlos';
          OMestre.Destinatario.Numero := '150';
          OMestre.Destinatario.Complemento := '';
          OMestre.Destinatario.CEP := '37730-000';
          OMestre.Destinatario.Bairro := 'Centro';
          OMestre.Destinatario.Municipio := 'Campestre';
          OMestre.Destinatario.UF := 'MG';
          OMestre.Destinatario.Telefone := '3537430000';
          OMestre.Destinatario.CodigoConsumidor := '1001'; // Código do Cliente
          OMestre.TipoAssinante := tac11182PessoaFisicaResidencial;
          OMestre.TipoUtilizacao := pc112ProvimentoAcessoInternet;
          OMestre.DataEmissao := StrToDate('01/01/2013');
          OMestre.Modelo := 21;
          OMestre.Serie := '001';
          OMestre.NumeroNF := I + 1;
          OMestre.ValorTotal := 400;
          OMestre.ICMS_BaseCalculo := 0;
          OMestre.ICMS_Valor := 0;
          OMestre.IsentosNaoTributadas := 0;
          OMestre.OutrosValores := 0;
          OMestre.IsentosNaoTributadas := 0;
          OMestre.AnoMesRefencia := '1212'; // AAMM
          for A := 0 to 3 do
          begin
            // Será informado apenas 1 item para exemplo, por isso o comando FOR está comentado
            // Informar todos os detalhes (Items) que compõem a NF (Serviço/Juros e etc)
            ODetalhe := TACBrConvenio115Item.Create;
            ODetalhe.CFOP := '5307';
            ODetalhe.Item := A + 1;
            ODetalhe.CodigoServico := '1' ;
            ODetalhe.DescricaoServico := 'Provimento de acesso a internet';
            ODetalhe.Unidade := '';
            ODetalhe.ClassificacaoItem := '1001' ; // Verificar tabela 11.5 do convênio
            ODetalhe.QtdeContratada := 0;
            ODetalhe.QtdePrestada := 0;
            ODetalhe.ValorTotal := 100;
            ODetalhe.Desconto := 0;
            ODetalhe.AcrescimosDespAcessorias := 0;
            ODetalhe.ICMSBaseCalculo := 0;
            ODetalhe.ICMSValor := 0;
            ODetalhe.ICMSAliquota := 0;
            ODetalhe.IsentoNaoTributados := 0;
            ODetalhe.OutrosValores := 0;
            ODetalhe.AnoMesApuracao := '1212';
            OMestre.Detalhes.Add(ODetalhe);
          end;
          Mestre.Add(OMestre);
        end;
      end;

      with Emitente do
      begin
        RazaoSocial := 'EMPRESA DE PROVIMENTO DE INTERNET';
        CNPJ := '12.345.678/0001-99';
        InscEstadual := '123456789';
        Endereco := 'Av. Junqueiras, 550';
        Cidade := 'Poços de Caldas';
        UF := 'MG';
        Telefone := '(35) 3721-1234';
        HomePage := 'http://www.jerasoft.com.br';
        EMail := 'suporte@jerasoft.com.br';
      end;
      Imprimir(True);
    end;
  finally
    FreeAndNil(FrmPRN);
  end;
end;

end.
