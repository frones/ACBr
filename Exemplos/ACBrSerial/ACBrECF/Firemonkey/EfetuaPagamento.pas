unit EfetuaPagamento;

interface

uses
  System.StrUtils, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation;

type
  TfrPagamento = class(TForm)
    PgPagamento: TTabControl;
    TabSheetPagamento: TTabItem;
    TabSheetEstorno: TTabItem;
    Label6: TLabel;
    Label7: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    edCod: TEdit;
    Label2: TLabel;
    edValor: TEdit;
    Label8: TLabel;
    edObs: TEdit;
    cbVinc: TCheckBox;
    Label4: TLabel;
    btImprimir: TButton;
    Label5: TLabel;
    lTotalPago: TLabel;
    lTotalAPAGAR: TLabel;
    mFormas: TMemo;
    SpeedButton1: TButton;
    btnEstornar: TButton;
    EdtMsgPromocional: TEdit;
    Label12: TLabel;
    EdtValor: TEdit;
    Label11: TLabel;
    CBVincNovo: TCheckBox;
    EdtTipoNovo: TEdit;
    Label10: TLabel;
    CbVincCancelado: TCheckBox;
    Label9: TLabel;
    EdtTipoCanc: TEdit;
    MemoInformacaoEstorno: TMemo;
    Label13: TLabel;
    MFormasEst: TMemo;
    procedure edValorKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure btImprimirClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure btnEstornarClick(Sender: TObject);
    procedure MFormasEstEnter(Sender: TObject);
    procedure mFormasEnter(Sender: TObject);
  private
    { Private declarations }
    procedure AtualizaVal ;
    Procedure CarregaFPG ;
  public
    { Public declarations }
    TipoCupom : Char ;
    Estado: String;
  end;

var
  frPagamento: TfrPagamento;

implementation

uses ECFTeste1, ACBrECFClass, ACBrUtil, ACBrConsts, ACBrECF;

{$R *.fmx}

procedure TfrPagamento.edValorKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if KeyChar in [',','.'] then
     KeyChar := DecimalSeparator ;
end;


procedure TfrPagamento.btImprimirClick(Sender: TObject);
begin
  if TipoCupom <> 'N' then
   begin
     Form1.ACBrECF1.EfetuaPagamento( edCod.Text, StrToFloat( edValor.Text),
                                     edObs.Text ,cbVinc.IsChecked );
     Form1.mResp.Lines.Add( 'Efetua Pagamento: '+edCod.Text +
                            ' Valor: '+edValor.Text +
                            ' Obs: '+edObs.Text +
                            ' Vinc: '+IfThen(cbVinc.IsChecked,'S','N') );
   end
  else
   begin
     Form1.ACBrECF1.EfetuaPagamentoNaoFiscal( edCod.Text, StrToFloat( edValor.Text),
                                     edObs.Text ,cbVinc.IsChecked );
     Form1.mResp.Lines.Add( 'Efetua Pagamento Não Fiscal: '+edCod.Text +
                            ' Valor: '+edValor.Text +
                            ' Obs: '+edObs.Text +
                            ' Vinc: '+IfThen(cbVinc.IsChecked,'S','N') );
   end ;

  Form1.AtualizaMemos ;
  AtualizaVal ;
end;

procedure TfrPagamento.FormShow(Sender: TObject);
begin
  if Estado = 'Estorno' then
    PgPagamento.ActiveTab:= TabSheetEstorno
  else
    PgPagamento.ActiveTab:= TabSheetPagamento;
  Estado:= '';

  AtualizaVal ;
  CarregaFPG ;
  TipoCupom := 'F' ;
end;

procedure TfrPagamento.AtualizaVal;
begin
  lTotalAPAGAR.Text := FloatToStr( Form1.ACBrECF1.Subtotal ) ;
  lTotalPago.Text   := FloatToStr( Form1.ACBrECF1.TotalPago ) ;
end;

procedure TfrPagamento.SpeedButton1Click(Sender: TObject);
Var Descricao : String ;
    FPG : TACBrECFFormaPagamento ;  { Necessita de uses ACBrECF }
begin
  if InputQuery('Pesquisa Descrição Forma Pagamento',
                'Entre com a Descrição a Localizar ou Cadastrar(Bematech)',
                Descricao) then
  begin
     FPG := Form1.ACBrECF1.AchaFPGDescricao( Descricao ) ;

     if FPG = nil then
        raise Exception.Create('Forma de Pagamento: '+Descricao+
                               ' não encontrada') ;

     edCod.Text := FPG.Indice ;

     { Bematech permite cadastrar formas de Pagamento dinamicamente }
     if (Form1.ACBrECF1.ModeloStr = 'Bematech') and
        (pos( FPG.Descricao, mFormas.Text ) = 0) then
        CarregaFPG ;
  end ;

end;

procedure TfrPagamento.CarregaFPG;
Var A : Integer ;
begin
  mFormas.Lines.Clear ;
  mFormas.Lines.Clear;
  with Form1 do
  begin
     { Bematech e NaoFiscal permitem cadastrar formas de Pagamento dinamicamente }
     if (Form1.ACBrECF1.Modelo in [ecfBematech,ecfNaoFiscal])then
        ACBrECF1.CarregaFormasPagamento
     else
        ACBrECF1.AchaFPGIndice('') ;  { força carregar, se ainda nao o fez }

     for A := 0 to ACBrECF1.FormasPagamento.Count -1 do
     begin
        mFormas.Lines.Add( ACBrECF1.FormasPagamento[A].Indice+' -> '+
              ACBrECF1.FormasPagamento[A].Descricao+' - '+IfThen(
              ACBrECF1.FormasPagamento[A].PermiteVinculado,'v',''));
        MFormasEst.Lines.Add( ACBrECF1.FormasPagamento[A].Indice+' -> '+
              ACBrECF1.FormasPagamento[A].Descricao+' - '+IfThen(
              ACBrECF1.FormasPagamento[A].PermiteVinculado,'v',''));
     end ;
  end ;

end;

procedure TfrPagamento.btnEstornarClick(Sender: TObject);
begin
   Form1.ACBrECF1.EstornaPagamento(EdtTipoCanc.Text, EdtTipoNovo.Text,
                                   StrToFloat(EdtValor.Text),EdtMsgPromocional.Text);
   Form1.AtualizaMemos ;
   AtualizaVal ;
end;

procedure TfrPagamento.MFormasEstEnter(Sender: TObject);
begin
   btnEstornar.SetFocus;
end;

procedure TfrPagamento.mFormasEnter(Sender: TObject);
begin
   btImprimir.SetFocus;
end;


end.
