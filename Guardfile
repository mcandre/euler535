guard :shell do
  watch(/^*\.hs$/) do |m|
    title = 'test'
    eager 'make'
    status = ($CHILD_STATUS.success? && :success) || :failed
    n '', title, status
    ''
  end
end
