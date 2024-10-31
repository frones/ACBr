package com.acbr.nfe.acbrlibnfe.demo.comandos;

import android.Manifest;
import android.content.Context;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Rect;
import android.graphics.pdf.PdfRenderer;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.os.ParcelFileDescriptor;
import android.util.Base64;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;

import com.acbr.nfe.acbrlibnfe.demo.utils.ACBrLibHelper;
import com.acbr.nfe.acbrlibnfe.demo.R;
import com.acbr.nfe.acbrlibnfe.demo.utils.NfeApplication;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ComandosEnvioNFeFragment extends Fragment {

    private NfeApplication application;
    private ACBrLibNFe ACBrNFe;

    private EditText txtNFeINI;
    private EditText txtNFeXML;
    private EditText txtRespostaEnvio;
    private EditText txtDestinatario;
    private Button btnLimparLista;
    private Button btnEnviarNFe;
    private Button btnImprimirNFCe;
    private Button btnImprimirPDFNFCe;
    private Button btnConverterDANFCePDFemBMP;
    private Button btnLimparRespostaEnvio;
    private Button btnEnviarEmailNFCe;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_envio_nfe, container, false);

        this.application = (NfeApplication)view.getContext().getApplicationContext();

        ACBrNFe = ACBrLibHelper.getInstance("ACBrLib.ini");

        checkAndRequestBluetoothPermission();

        txtNFeINI = view.findViewById(R.id.txtNFeINI);
        txtNFeXML = view.findViewById(R.id.txtNFeXML);
        txtDestinatario = view.findViewById(R.id.txtDestinatario);
        txtRespostaEnvio = view.findViewById(R.id.txtRespostaEnvio);
        btnLimparLista = view.findViewById(R.id.btnLimparLista);
        btnEnviarNFe = view.findViewById(R.id.btnEnviarNFe);
        btnEnviarEmailNFCe = view.findViewById(R.id.btnEnviarEmailNFCe);
        btnImprimirNFCe = view.findViewById(R.id.btnImprimirNFCe);
        btnImprimirPDFNFCe = view.findViewById(R.id.btnImprimirPDFNFCe);
        btnConverterDANFCePDFemBMP = view.findViewById(R.id.btnConverterDANFCePDFemBMP);
        btnLimparRespostaEnvio = view.findViewById(R.id.btnLimparRespostaEnvio);

        btnLimparLista.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                limparListaNFe();
            }
        });

        btnEnviarNFe.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                enviarNFe();
            }
        });

        btnEnviarEmailNFCe.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                enviarEmailNFCe();
            }
        });

        btnImprimirNFCe.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                imprimirNFCe();
            }
        });

        btnImprimirPDFNFCe.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                imprimirPDFNFCe();
            }
        });

        btnConverterDANFCePDFemBMP.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                converterNFCePDFemBMP();
            }
        });

        btnLimparRespostaEnvio.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaEnvio();
            }
        });

        return view;
    }

    public void checkAndRequestBluetoothPermission() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            if (requireContext().checkSelfPermission(Manifest.permission.BLUETOOTH_CONNECT) != PackageManager.PERMISSION_GRANTED ||
                    requireContext().checkSelfPermission(Manifest.permission.BLUETOOTH_SCAN) != PackageManager.PERMISSION_GRANTED) {
                requestPermissions(new String[]{
                        Manifest.permission.BLUETOOTH_CONNECT,
                        Manifest.permission.BLUETOOTH_SCAN
                }, 1001);
            }
        } else {
            if (requireContext().checkSelfPermission(Manifest.permission.BLUETOOTH) != PackageManager.PERMISSION_GRANTED ||
                    requireContext().checkSelfPermission(Manifest.permission.BLUETOOTH_ADMIN) != PackageManager.PERMISSION_GRANTED) {
                requestPermissions(new String[]{
                        Manifest.permission.BLUETOOTH,
                        Manifest.permission.BLUETOOTH_ADMIN
                }, 1001);
            }
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);

        if (requestCode == 1001) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                Log.d("Permissions", "Permissões de Bluetooth concedidas.");
                // Se você tem código que depende da permissão, pode chamá-lo aqui
            } else {
                Log.d("Permissions", "Permissões de Bluetooth negadas.");
            }
        }
    }

    public void limparListaNFe() {
        txtRespostaEnvio.setText("");
        try {
            ACBrNFe.LimparLista();
            txtRespostaEnvio.setText("Método executado com sucesso !!");
        } catch (Exception ex) {
            Log.e("Erro ao Limpar Lista NFe", ex.getMessage());
            txtRespostaEnvio.setText(ex.getMessage());
        }
    }

    public void enviarNFe() {
        txtRespostaEnvio.setText("");
        String result = "";
        String NFeIni = txtNFeINI.getText().toString();
        try {
            ACBrNFe.CarregarINI(NFeIni);
            result = ACBrNFe.Enviar(1, false, false, false);
        } catch (Exception ex) {
            Log.e("Erro ao Enviar NFe", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaEnvio.setText(result);
        }
    }

    public void enviarEmailNFCe() {
        txtRespostaEnvio.setText("");
        String NFeXml = txtNFeXML.getText().toString();
        String destinatario = txtDestinatario.getText().toString();
        try {
            ACBrNFe.EnviarEmail(destinatario, NFeXml, true, "Envio NFCe Email", "", "", "Envio NFCe");
            txtRespostaEnvio.setText("Email enviado com sucesso!");
        } catch (Exception ex) {
            Log.e("Erro ao Enviar Email", ex.getMessage());
            txtRespostaEnvio.setText(ex.getMessage());
        }
    }

    public void imprimirNFCe() {
        // Verifica se as permissões necessárias foram concedidas
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            if (requireContext().checkSelfPermission(Manifest.permission.BLUETOOTH_CONNECT) != PackageManager.PERMISSION_GRANTED ||
                    requireContext().checkSelfPermission(Manifest.permission.BLUETOOTH_SCAN) != PackageManager.PERMISSION_GRANTED) {
                Log.d("Permissions", "Permissões de Bluetooth não concedidas.");
                return; // Não continua se a permissão não foi concedida
            }
        } else {
            if (requireContext().checkSelfPermission(Manifest.permission.BLUETOOTH) != PackageManager.PERMISSION_GRANTED ||
                    requireContext().checkSelfPermission(Manifest.permission.BLUETOOTH_ADMIN) != PackageManager.PERMISSION_GRANTED) {
                Log.d("Permissions", "Permissões de Bluetooth não concedidas.");
                return; // Não continua se a permissão não foi concedida
            }
        }

        try {
            Log.d("Print", "Limpando lista antes da impressão.");
            ACBrNFe.LimparLista();

            String xmlContent = txtNFeXML.getText().toString();
            if (xmlContent.isEmpty()) {
                Log.d("Print", "XML está vazio. Não pode carregar.");
                return;
            }

            Log.d("Print", "Carregando XML: " + xmlContent);
            ACBrNFe.CarregarXML(xmlContent);
            Log.d("Print", "Preparando para imprimir.");
            ACBrNFe.Imprimir("", 1, "False", "False", "False", "False", "False");
            Log.d("Print", "Impressão realizada com sucesso.");
        } catch (Exception ex) {
            Log.e("Erro ao Imprimir NFCe", ex.getMessage());
            ex.printStackTrace();  // Para ver a pilha de erros
            txtRespostaEnvio.setText(ex.getMessage());
        }
    }

    public void imprimirPDFNFCe(){
        try {
            Log.d("Print", "Limpando lista antes da impressão.");
            ACBrNFe.LimparLista();

            String xmlContent = txtNFeXML.getText().toString();
            if (xmlContent.isEmpty()) {
                Log.d("Print", "XML está vazio. Não pode carregar.");
                return;
            }

            Log.d("Print", "Carregando XML: " + xmlContent);
            ACBrNFe.CarregarXML(xmlContent);
            Log.d("Print", "Preparando para imprimir.");
            ACBrNFe.ImprimirPDF();
            Log.d("Print", "Impressão PDF realizada com sucesso.");
        } catch (Exception ex) {
            Log.e("Erro ao Imprimir NFCe", ex.getMessage());
            ex.printStackTrace();  // Para ver a pilha de erros
            txtRespostaEnvio.setText(ex.getMessage());
        }
    }

    public void converterNFCePDFemBMP() {
        try {
            Log.d("Print", "Limpando lista antes da impressão.");
            ACBrNFe.LimparLista();

            String xmlContent = txtNFeXML.getText().toString();
            if (xmlContent.isEmpty()) {
                Log.d("Print", "XML está vazio. Não pode carregar.");
                return;
            }

            Log.d("Print", "Carregando XML: " + xmlContent);
            ACBrNFe.CarregarXML(xmlContent);

            Log.d("Print", "Preparando para imprimir.");
            ACBrNFe.ImprimirPDF();

            // Caminho do PDF gerado
            File pdfFile = new File(requireActivity().getExternalFilesDir("pdf"), "nfc_e.pdf");

            // Verifica se o arquivo PDF foi criado com sucesso
            if (!pdfFile.exists()) {
                Log.e("Erro", "PDF não encontrado no caminho: " + pdfFile.getAbsolutePath());
                return;
            }

            // Converte o PDF para Bitmap
            int targetWidth = 320; // Largura em pixels.
            int targetHeight = 500; // Altura em pixels.
            Bitmap bitmap = savePdfAndConvertToBitmap(requireActivity(), pdfFile.getAbsolutePath(), targetWidth, targetHeight);
            if (bitmap == null) {
                Log.e("Erro ao Imprimir NFCe", "Bitmap está nulo. Conversão de PDF para Bitmap falhou.");
                return;
            }

            saveBitmapToFile(bitmap);

        } catch (Exception ex) {
            String errorMessage = (ex.getMessage() != null) ? ex.getMessage() : "Erro desconhecido";
            Log.e("Erro ao Imprimir NFCe", errorMessage, ex);
            txtRespostaEnvio.setText(errorMessage);
        }
    }

    private Bitmap savePdfAndConvertToBitmap(Context context, String pdfPath, int targetWidth, int targetHeight) {
        File pdfFile = new File(pdfPath);

        if (!pdfFile.exists()) {
            Log.e("Erro", "Arquivo PDF não encontrado: " + pdfPath);
            return null;
        }

        try (ParcelFileDescriptor fileDescriptor = ParcelFileDescriptor.open(pdfFile, ParcelFileDescriptor.MODE_READ_ONLY);
             PdfRenderer pdfRenderer = new PdfRenderer(fileDescriptor)) {

            if (pdfRenderer.getPageCount() == 0) {
                Log.e("Erro", "O PDF não contém páginas.");
                return null;
            }

            PdfRenderer.Page page = pdfRenderer.openPage(0);

            // Cria um Bitmap com o fundo branco
            Bitmap bitmap = Bitmap.createBitmap(targetWidth, targetHeight, Bitmap.Config.ARGB_8888);
            Canvas canvas = new Canvas(bitmap);
            canvas.drawColor(Color.WHITE); // Define o fundo como branco

            // Renderiza o conteúdo do PDF no bitmap redimensionado
            Rect rect = new Rect(0, 0, page.getWidth(), page.getHeight()); // Zera as margens
            page.render(bitmap, rect, null, PdfRenderer.Page.RENDER_MODE_FOR_DISPLAY);

            // Fecha a página após a renderização
            page.close();
            Log.d("Imagem BMP", "Imagem convertida com fundo branco para impressão.");

            return bitmap;

        } catch (Exception e) {
            Log.e("Erro ao renderizar PDF", e.getMessage(), e);
            return null;
        }
    }

    private void saveBitmapToFile(Bitmap bitmap) {
        if (bitmap == null) {
            Log.e("Erro ao Imprimir NFCe", "Bitmap recebido é nulo.");
            return;
        }

        File directory = new File(application.getBmpPath());

        if (!directory.exists() && !directory.mkdirs()) {
            Log.e("Erro", "Falha ao criar diretório: " + directory.getAbsolutePath());
            return;
        }

        String fileName = "imagem_convertida.bmp";
        File file = new File(directory, fileName);

        try (FileOutputStream fos = new FileOutputStream(file)) {
            if (!bitmap.compress(Bitmap.CompressFormat.PNG, 100, fos)) {
                Log.e("Erro ao Salvar Imagem", "Falha ao comprimir e salvar o bitmap.");
                return;
            }
            Log.d("Salvar Imagem", "Imagem salva em: " + file.getAbsolutePath());
            txtRespostaEnvio.setText("Imagem salva com sucesso em: " + file.getAbsolutePath());
        } catch (IOException e) {
            Log.e("Erro ao Salvar Imagem", "Erro: " + e.getMessage(), e);
        }
    }

    public void LimparRespostaEnvio() {
        txtRespostaEnvio.setText("");
    }
}