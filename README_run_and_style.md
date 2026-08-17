# R 作图脚本使用说明

## 1. 运行

在项目根目录打开 R 或 PowerShell。

R 控制台中运行：

    source("outputs/customer_mea_co2_analysis.R")

或 PowerShell 中运行：

    & "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" "outputs\customer_mea_co2_analysis.R"

脚本会读取顶部的 input_file 指向的 Excel 数据，并在 outputs 文件夹生成 TIFF、PDF、CSV 指标及 SHAP 明细。

## 2. 修改数据文件路径

在脚本开头找到：

    input_file <- "..."

将引号中的地址替换为新的 Excel 文件完整路径。Excel 的列结构需保持原样：第二行是字段名，且包含 Time、Temperature、MEA conc、CO2 loading、三个输出变量。

## 3. 统一调整字体与字号

脚本顶部有 CENTRAL TYPOGRAPHY AND EXPORT SETTINGS 参数区。常用参数：

    FONT_FAMILY <- "Arial"              # 全部文字字体
    FONT_AXIS_TITLE_PT <- 13            # 坐标轴标题
    FONT_AXIS_TICK_PT <- 11             # 坐标轴刻度数字/模型名/变量名
    FONT_FIGURE_TITLE_PT <- 16          # 每张图总标题
    FONT_FIGURE_SUBTITLE_PT <- 12       # 总副标题
    FONT_PANEL_TITLE_PT <- 13           # 面板标题
    FONT_ANNOTATION_PT <- 11            # n = ... 样本量标注
    FONT_BAR_VALUE_PT <- 10             # 柱顶指标数字
    FONT_LEGEND_TITLE_PT <- 12          # SHAP 色带标题
    FONT_LEGEND_LABEL_PT <- 11          # SHAP 色带 High / Low
    FONT_CAPTION_PT <- 9                # 图注

例如，若客户要求所有轴刻度数字也为 13 pt，只改：

    FONT_AXIS_TICK_PT <- 13

无需在各图代码中逐一修改。

## 4. 导出格式与清晰度

    TIFF_DPI <- 300

此参数控制 TIFF 分辨率。脚本对每张图自动生成：

- .tiff：300 dpi、LZW 压缩，适合投稿位图图件。
- .pdf：矢量格式，适合文字和线条保持清晰。

导出尺寸在各 export_figure() 调用中指定，单位为英寸。例如：

    export_figure(p_shap, "04_shap_beeswarm_capture_energy_xgb_extratrees", 12.5, 10.5)

最后两个数字分别为宽度和高度；只有期刊给出版面宽度时才建议修改。

## 5. 模型设置

CO2 capture efficiency 使用：

    XGBoost, Random Forest, Extra Trees, Gaussian Process Regression

Regeneration energy 使用：

    Linear Regression, Random Forest, SVR, Gaussian Process Regression

模型列表位于 models_by_target。所有指标均使用 LOOCV。每个留一折中，输入变量先在训练折计算均值和标准差，再应用于训练与测试数据；因此 SVR 已正确使用 StandardScaler，且不泄漏测试数据。

SHAP 自动选择每个目标中 LOOCV R2 最高的模型：

    best_models <- metrics |> group_by(Target) |> slice_max(R2, n = 1, with_ties = FALSE)

## 6. 输出文件

最终投稿文件以 .tiff 和 .pdf 为准。生成后请确认：

- 图1：输入变量箱线图
- 图2：输出变量箱线图
- 图3：模型指标对比
- 图4：SHAP 蜂群图

原始 Excel 文件不会被修改。脚本仅在 outputs 中写入清洗后的分析副本和结果文件。

