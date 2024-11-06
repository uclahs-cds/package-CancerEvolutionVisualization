test_that('check.file.extension works correctly', {
    supported.ext <- c('png', 'tiff')
    default.ext <- 'tiff'

    result <- check.file.extension(
        'test.png',
        supported.ext,
        default.ext
        );

    expect_equal(result$name, 'test.png')
    expect_equal(result$ext, 'png')

    result <- check.file.extension(
        'test.tiff',
        supported.ext,
        default.ext
        );
    expect_equal(result$name, 'test.tiff')
    expect_equal(result$ext, 'tiff')


    expect_message(
        result <- check.file.extension(
            'test.txt',
            supported.ext,
            default.ext
            ),
        'File extension txt is not in the list of supported extensions. Switching to default extension tiff'
    )
    expect_equal(result$name, 'test.tiff')
    expect_equal(result$ext, 'tiff')
    });
