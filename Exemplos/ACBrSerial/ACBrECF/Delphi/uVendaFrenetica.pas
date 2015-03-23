////////////////////////////////////////////////////////////////////////////////
//
// Data: Setembro - 2013
// Autor: Luiz Paulo Ferrari
// Baseado em códigos propostos no Fórum ACBr por Daniel Simões.
//
// Adicionado ao Exemplo do ACBrECF em 02/01/2015
// com pequenas alterações por Elton (EMBarbosa)
////////////////////////////////////////////////////////////////////////////////

unit uVendaFrenetica;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrBase, ACBrLCB, ExtCtrls, ACbrDevice;

type
  TFrVendaFrenetica = class(TForm)
    edCodProduto: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    TimerVendeItem: TTimer;
    ACBrLCB1: TACBrLCB;
    btnCancelarCupom: TButton;
    chkProcessMessages: TCheckBox;
    btnSair: TButton;
    chkNaoExibirMsgs: TCheckBox;
    btnFechaCupom: TButton;
    lblInfo: TLabel;
    procedure edCodProdutoKeyPress(Sender: TObject; var Key: Char);
    procedure TimerVendeItemTimer(Sender: TObject);
    procedure ACBrLCB1LeCodigo(Sender: TObject);
    procedure VendeItem(CodBarras: String);
    procedure btnCancelarCupomClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSairClick(Sender: TObject);
    procedure btnFechaCupomClick(Sender: TObject);
    procedure chkProcessMessagesClick(Sender: TObject);
  private
    { Private declarations }
    FVendendo: Boolean;
    procedure AbrirCupom;
  public
    { Public declarations }
  end;

var
  FrVendaFrenetica: TFrVendaFrenetica;

implementation

uses ECFTeste1;

{$R *.dfm}

procedure TFrVendaFrenetica.edCodProdutoKeyPress(Sender: TObject; var Key: Char);
Var
  CodBarras: String;
begin
  if (Key = #13) or (Key = #10) then // ENTER (CR) ou LF
  begin
    CodBarras := Trim(edCodProduto.Text);

    //Adiciona o produto a lista de produtos a serem processados
    ACBrLCB1.Fila.Add(CodBarras);

    // Zera o Edit, preparando para uma nova venda rapidamente
    edCodProduto.Text := '';
    edCodProduto.SetFocus;

    // Liga o Timer responsável por efetuar as vendas
    TimerVendeItem.Enabled := True;
  end;
end;

procedure TFrVendaFrenetica.TimerVendeItemTimer(Sender: TObject);
Var
  CodBarras: String;
begin
  TimerVendeItem.Enabled := False;
  try
    // Verifica se o ACBrECF está ocupado, Se estiver não vende, tentará novamente no próximo evento do Timer //
    // FVendendo é liga no inicio de VendeItem e desligada no Final

    if not(FVendendo or Form1.ACBrECF1.AguardandoResposta) then
    begin
      CodBarras := ACBrLCB1.LerFila;
      VendeItem(CodBarras);
    end;
  finally
    TimerVendeItem.Enabled := (ACBrLCB1.Fila.Count > 0);
  end;
end;

procedure TFrVendaFrenetica.ACBrLCB1LeCodigo(Sender: TObject);
begin
  TimerVendeItem.Enabled := True
end;

procedure TFrVendaFrenetica.VendeItem(CodBarras: String);
begin
  FVendendo := True;

  try
    if Trim(CodBarras) = '' then
      Exit;

    if (Form1.ACBrECF1.Estado in [estLivre]) then
    begin
      AbrirCupom;
    end;

    Form1.ACBrECF1.VendeItem(CodBarras, 'DESCRICAO DO PRODUTO', 'NN', 1, 1.5, 0, 'UN', '$');

    Form1.mResp.Lines.Add('Vende Item: Cod:'+ CodBarras+
                          ' Desc: DESCRICAO DO PRODUTO'+
                          ' Aliq: NN' +
                          ' Qtd: 1' +
                          ' Preço: 1.50' +
                          ' Desc: 0' +
                          ' Un: UN' +
                          ' Desc: $');
    Form1.AtualizaMemos;
  finally
    edCodProduto.SetFocus;
    FVendendo := False;
  end;
end;

procedure TFrVendaFrenetica.btnCancelarCupomClick(Sender: TObject);
begin
  try
    btnFechaCupom.Enabled := False;
    btnCancelarCupom.Enabled := False;

    Form1.ACBrECF1.CancelaCupom;
    Form1.mResp.Lines.Add('CancelaCupom');
    Form1.AtualizaMemos;

    edCodProduto.SetFocus;
  Finally
    btnFechaCupom.Enabled := True;
    btnCancelarCupom.Enabled := True;
  end;
end;

procedure TFrVendaFrenetica.FormShow(Sender: TObject);
begin

  if not(Form1.ACBrECF1.Ativo) then
  begin
    MessageDlg('ECF não Ativo.', mtInformation, [mbOk], 0);
  end;

  // Opcionalmente diminuir o IntervaloAposComando;
  //form1.ACBrECF1.IntervaloAposComando := 0;

  edCodProduto.SetFocus;
end;

procedure TFrVendaFrenetica.btnSairClick(Sender: TObject);
begin
  Close;
end;

procedure TFrVendaFrenetica.btnFechaCupomClick(Sender: TObject);
begin
  btnFechaCupom.Enabled := False;
  btnCancelarCupom.Enabled := False;

  try
    Form1.ACBrECF1.SubtotalizaCupom(0, '');
    Form1.mResp.Lines.Add('Subtotalizado Cupom');

    Form1.ACBrECF1.EfetuaPagamento(Form1.ACBrECF1.FormasPagamento[0].Indice,
      (Form1.ACBrECF1.Subtotal - Form1.ACBrECF1.TotalPago), 'ZERANDO SALDO A PAGAR RESTANTE');
    Form1.mResp.Lines.Add('Efetuado Pagamento');

    Form1.ACBrECF1.FechaCupom('TESTE DE CUPOM');
    Form1.mResp.Lines.Add('Finalizado Cupom Fiscal');

    edCodProduto.SetFocus;
  Finally
    btnFechaCupom.Enabled := True;
    btnCancelarCupom.Enabled := True;
  end;
end;

procedure TFrVendaFrenetica.chkProcessMessagesClick(Sender: TObject);
begin
  Form1.ACBrECF1.Device.ProcessMessages := chkProcessMessages.checked;
end;

procedure TFrVendaFrenetica.AbrirCupom;
var
  b: Boolean;
begin
  b := Form1.ACBrECF1.ExibeMensagem;
  if (chkNaoExibirMsgs.checked) and (b) then
    Form1.ACBrECF1.ExibeMensagem := False;
  Form1.ACBrECF1.AbreCupom();
  if (chkNaoExibirMsgs.checked) and (b) then
    Form1.ACBrECF1.ExibeMensagem := True;
end;

end.
