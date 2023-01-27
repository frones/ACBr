unit uFrameVendaProduto;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts;

type
  TFrameVendaProduto = class(TFrame)
    Rectangle1: TRectangle;
    Text1: TText;
    Total: TText;
    ImagemProduto: TImage;
    Layout1: TLayout;
    DescricaoProduto: TText;
    PrecoUnitario: TText;
    QuantidadeProduto: TText;
  private
    FDescricao: String;
    FValorUnitario: Double;
    FQuantidade: Single;
    FTotalVenda: Double;
    procedure SetDescricao(const Value: String);
    procedure SetQuantidade(const Value: Single);
    procedure SetValorUnitario(const Value: Double);
    procedure SetTotalVenda(const Value: Double);
    function GetImagem: TBitmap;
    procedure SetImagem(const Value: TBitmap);
  public
    Procedure Show;
    Procedure Hide;
    property Imagem: TBitmap Read GetImagem Write SetImagem;
    property Descricao: String Read FDescricao Write SetDescricao;
    property Quantidade: Single Read FQuantidade Write SetQuantidade;
    property ValorUnitario: Double Read FValorUnitario Write SetValorUnitario;
    property TotalVenda: Double Read FTotalVenda Write SetTotalVenda;
  end;

implementation

Uses
  uSecondDisplay;

{$R *.fmx}
{ TFrame1 }

Function TFrameVendaProduto.GetImagem: TBitmap;
begin
  Result := ImagemProduto.Bitmap;
end;

procedure TFrameVendaProduto.SetDescricao(const Value: String);
begin
  FDescricao := Value;
  DescricaoProduto.Text := Value;
end;

procedure TFrameVendaProduto.SetImagem(const Value: TBitmap);
begin
  ImagemProduto.Bitmap.Assign(Value);
end;

procedure TFrameVendaProduto.SetQuantidade(const Value: Single);
begin
  FQuantidade := Value;
  QuantidadeProduto.Text := FormatFloat('###,##0.000 X', Value);
end;

procedure TFrameVendaProduto.SetTotalVenda(const Value: Double);
begin
  FTotalVenda := Value;
  Total.Text := 'R$ ' + FormatFloat('#,###,##0.00', Value).Trim;
end;

procedure TFrameVendaProduto.SetValorUnitario(const Value: Double);
begin
  FValorUnitario := Value;
  PrecoUnitario.Text := FormatFloat('R$ #,###,##0.00', Value);
end;

procedure TFrameVendaProduto.Show;
begin
  FormSecondDisplay.ShowLayout(Self);
end;

procedure TFrameVendaProduto.Hide;
begin
  FormSecondDisplay.HideLayout(Self);
end;

end.
